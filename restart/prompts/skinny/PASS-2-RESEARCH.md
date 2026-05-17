# SKINNY PASS 2 — S-P2 RESEARCH (SOTA Grounding + Primitive Design)

S-P2 is the **research pass** of the skinny track. It is the empirical
counterpart of the totality T-P2 Research: where T-P2 designs the
greater-spec architecture, S-P2 grounds the JSON engine against the
state of the art and designs the candidate primitives. S-P2 consumes
the S-P1 profile — every hot leaf S-P1 named — and produces six grounded
research artefacts that name, for each candidate primitive, its SOTA
antecedent, its scalar-reference shape, its checkasm-parity discipline,
and its grammar-neutral generalisation. S-P2 selects nothing and
sequences nothing; that is S-P3's work.

The pass is **iterative + auto-convergent**. Six parallel sub-agents
P2-A–P2-F fan out per the scope matrix in §2. A six-lens CHALLENGE wave
adversarially reviews the output per `ORCHESTRATOR.md` §3W.
Dispositions fold into V{N+1}. The loop terminates at the convergence
criterion in `ORCHESTRATOR.md` §3Z. Re-execution is composable.

## §1 — Trigger + entry condition

S-P2 dispatches when all of the following hold:

- **S-P1 converged.** The S-P1 CHALLENGE returned ≥95% ACCEPT for two
  consecutive cycles; the `restart/skinny/tranches/sk-v{N}/HANDOFF.md`
  next-move line reads `ready-for-S-P2`.
- **The profile is current.** `restart/skinny/tranches/sk-v{N}/research/p1/`
  carries the six committed P1 artefacts plus the consolidation; every
  hot leaf is resolved to a symbol.
- **The profile names the new baseline.** Per the profile-first
  non-negotiable in `ORCHESTRATOR.md` §8, S-P2 grounds its design on
  the SK-V{N} hot leaves only — no hypothesis carries over from
  SK-V{N-1} without a fresh P1 hot-leaf antecedent.

Or the user explicitly invokes `dispatch S-P2 SK-V{N}`.

S-P2 is read-only against `skinny/` source. It produces design
artefacts, not source edits — implementation lands only inside the wave
triumvirate's redress phase, post-S-P3.

## §2 — Scope matrix (six parallel sub-agents)

Output root: `restart/skinny/tranches/sk-v{N}/research/p2/`. Each agent
writes ONE artefact at the assigned path, overwritten in place each
cycle. Hard cap 45 min per agent.

| Agent | Scope | Output |
|---|---|---|
| **P2-A SOTA comparator teardown** | Architecture teardown of asmjson, sonic-rs, simdjson, yyjson. For each: structural-classification strategy, number/string fast paths, tape/DOM/on-demand output plane, and the strict-vs-strict comparator discipline. Name precisely what each does that bbnf does not — keyed to the S-P1 hot leaves. | `p2/p2a-sota-teardown.md` |
| **P2-B DAV1D/FFmpeg ASM process** | The hand-written-ASM SIMD process from dav1d / FFmpeg / VLC: the scalar-oracle-first discipline, the checkasm differential harness, and the same-wave-consumer rule. Map that process onto bbnf-simd's `checkasm_*` tests + the scalar reference requirement. Produce the primitive-admission process S-P3 will gate against. | `p2/p2b-dav1d-process.md` |
| **P2-C host-arch ASM/SIMD esoterica** | Host-architecture instruction inventory. Primary: aarch64 — PMULL, CSSC CTZ, UDOT/DotProd, TBL/TBX, the wide-shift family. Secondary: x86 — AVX2, AVX-512, GFNI, VBMI2, VPCLMUL. Note esoteric or underexploited instructions worth leveraging against the S-P1 hot leaves; flag REDRESS-blocked instruction routes. | `p2/p2c-arch-esoterica.md` |
| **P2-D substrate + tape design** | Interrogate the offset-tape substrate: the lazy-materialisation counters from `skinny/RESULTS.md`, the logical-vs-allocated tape ratios, the structural-projection union. Whether tape and structural projection are one substrate (Lock 1) and where a tape-shape change would move a hot leaf. No parallel substrate proposals. | `p2/p2d-substrate-tape.md` |
| **P2-E parse-that primitive gaps** | parse-that's primitive vocabulary: which SIMD / string / float / regex primitives the S-P1 hot leaves demand that parse-that does not yet expose. Per gap: the missing primitive's shape, its scalar reference sketch, its Layer-0/Layer-1 placement in the bbnf-simd two-layer vocabulary. | `p2/p2e-parse-that-gaps.md` |
| **P2-F grammar-neutral abstraction** | For every candidate primitive surfaced by P2-B/C/D/E, the grammar-neutral abstraction: how the primitive generalises beyond JSON to CSS L4 / Sheets / BBNF-self (Lock 14). A primitive that cannot be expressed as a grammar-neutral byte-set / classifier / tape operation is flagged for S-P3 as JSON-overfit. | `p2/p2f-grammar-neutral.md` |

Each agent reads, before producing its artefact: the six P1 artefacts
under `research/p1/`, `skinny/RESULTS.md`, `skinny/REDRESS.md`,
`restart/skinny/tranches/sk-v{N}/HANDOFF.md`, the locks at
`restart/locks/LOCKS.md` (Locks 1 and 14 are load-bearing here), and
this prompt end-to-end. P2-F additionally reads P2-B/C/D/E output in the
CHALLENGE-fold cycle.

### §2.1 — Per-agent output-schema frontmatter

Every P2 artefact opens with this frontmatter block:

```markdown
# SK-V{N} P2-{X}: {Topic}

Pass: S-P2 Research. Cycle: V{N}.
Date: YYYY-MM-DD.
Scope: {one-line scope spec}.
Output: this file.
P1 hot-leaf antecedents: {the named S-P1 hot leaves this artefact grounds against}.
Lock surface: {Lock 1 / Lock 14 / both — which the design touches}.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)
## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)
## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)
## §4 — Risks (REDRESS entries any candidate must NOT re-open)
## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)
```

The §2 candidate-primitive enumeration is the load-bearing artefact: a
research agent that returns architectural prose without a concrete
candidate list fails CH1 of the CHALLENGE wave.

## §3 — Six-lens CHALLENGE pass (CH1–CH6 specialised to S-P2)

After all six P2 artefacts commit, the CHALLENGE wave dispatches per
`ORCHESTRATOR.md` §3W. Six lens agents fan out; each writes
`p2/hardening/V{N}/CH{n}.md`; one aggregator writes
`p2/hardening/HARDENING-S-P2-V{N}-CONSOLIDATED.md`. Disposition
vocabulary is ACCEPT / REVISE / REJECT.

**CH1 CORRECTNESS** — does every candidate primitive trace to a named
S-P1 hot leaf? A candidate with no P1 antecedent is a speculative
kernel — REJECT. Are SOTA-comparator claims cited against the correct
source (asmjson / sonic-rs / simdjson / yyjson) and the correct
strictness plane? Are ISA claims cited against the architecture
reference manual?

**CH2 GENERALITY** — does every candidate carry a P2-F grammar-neutral
verdict? Lock 14 holds: a primitive proposed only because JSON needs it,
with no grammar-neutral byte-set / classifier / tape expression, is
JSON-overfit and CH2 marks it REVISE (re-express) or REJECT (drop). The
candidate must work for CSS L4 / Sheets / BBNF-self or be re-framed as a
per-grammar template surface.

**CH3 REGRESSION** — does any candidate re-open a route in
`skinny/REDRESS.md`? The pre-block surface includes REDRESS 28+33 (Class
A NEON tiny-string wiring), 50-55 (SK-V5 UTF-8 fusion), 60-72 (SK-V6
retained-parse + sidecar producers + digest cap-16), 80 (canada
mantissa-widen), 82-84 (single-quartet unicode classifier,
StringBlock16 tiny probe, object-pair compaction), 88 (PMULL prefix-XOR
as hot body), 89 (CSSC CTZ next-bit bulk consumer), plus the historical
blocked routes. A candidate that re-opens one without fresh P1
evidence + a new framing is REJECTed.

**CH4 COST** — does each candidate carry a scalar-reference status, a
checkasm-parity expectation, and a same-wave-consumer note? Per the
non-negotiables in `ORCHESTRATOR.md` §8, no SIMD/ASM primitive ships
without a scalar reference and checkasm parity before wiring, and no
kernel ships without a same-wave consumer. A candidate missing any of
the three fails CH4.

**CH5 HIDDEN COUPLING** — does any candidate introduce a parallel
substrate, a sidecar producer, a renamed scanner, or a Track 1 ≡ Track 2
dishonesty? P2-D's tape interrogation must conclude the substrate union
holds (Lock 1); a candidate that proposes a second source scan, a
retained cursor, an aux density table, or a parser-owned structural
projection violates Lock 1 and CH5 REJECTs it.

**CH6 ANTI-PAPER-CLOSE** — no agent's self-report of "researched" or
"designed" stands without orchestrator-citable evidence: a comparator
claim needs the comparator source file, an ISA claim needs the manual
section, a primitive claim needs the scalar-reference sketch in §2. A
candidate deferred to "a future wave will detail" is a paper-close — the
research either grounds the candidate now or drops it.

The lens registry is monotonically extensible per `ORCHESTRATOR.md`
§3W; add CH7+ if S-P2 surfaces a failure mode CH1–CH6 cannot
disposition.

## §4 — Iteration + auto-convergence

S-P2 executes cycles V1, V2, V3, … per `ORCHESTRATOR.md` §3Z, with a
per-pass independent cycle counter.

Per cycle: (1) the six P2 agents dispatch and commit; (2) the CHALLENGE
wave dispatches; (3) the aggregator produces the consolidation with the
ACCEPT-rate and the REVISE/REJECT lists; (4) every disposition folds
into the V{N+1} dispatch — hardening without folding is paper-hardening
and the orchestrator does not advance.

**Convergence criterion.** S-P2 advances to S-P3 when CHALLENGE returns
**≥95% ACCEPT for two consecutive cycles**, with zero open critical
defects and no orphan unresolved REVISE; or the user pins the cycle
final at sign-off (§6).

**Hard ceiling.** V ≤ 5. An S-P2 reaching V5 without convergence
escalates to the user with a `BLOCKED` verdict naming the unresolved
REVISE dispositions — typically a candidate that survives neither the
grammar-neutrality test nor the REDRESS pre-block.

## §5 — Output structure

```
restart/skinny/tranches/sk-v{N}/research/p2/
├── p2a-sota-teardown.md
├── p2b-dav1d-process.md
├── p2c-arch-esoterica.md
├── p2d-substrate-tape.md
├── p2e-parse-that-gaps.md
├── p2f-grammar-neutral.md
└── hardening/
    ├── V{N}/
    │   ├── CH1.md  CH2.md  CH3.md
    │   ├── CH4.md  CH5.md  CH6.md
    └── HARDENING-S-P2-V{N}-CONSOLIDATED.md
```

## §6 — Sign-off + hand-on

S-P2 has no mandatory user gate of its own. On convergence the
orchestrator:

1. Reads the six P2 artefacts + the consolidation end-to-end.
2. Updates `restart/skinny/tranches/sk-v{N}/HANDOFF.md`: next-move line
   becomes `ready-for-S-P3`.
3. Dispatches S-P3 Synthesis-Plan per `skinny/PASS-3-SYNTHESIS-PLAN.md`.

S-P3 consumes the S-P2 research as its candidate pool: the P3-A
shortlist is drawn only from candidates that survived the S-P2
CHALLENGE. A candidate REJECTed by S-P2's CH1–CH6 is not eligible for
the S-P3 shortlist. The chain is **S-P1 (measure) → S-P2 (ground SOTA +
design primitives) → S-P3 (synthesise the wave plan) → wave triumvirate
(execute)** — S-P2 mirrors the totality T-P2 Research structurally.

## §7 — Hard caps

| Phase | Wall budget |
|---|---|
| Six P2 agents (parallel) | 45 min per agent; ~60 min wall incl. commit |
| CHALLENGE wave (6 + 1 consolidation) | ~90 min wall |
| Per cycle total | ~2.5 hours wall |
| Whole pass (V ≤ 5) | ceiling ~12 hours wall |

Every dispatch carries an explicit minute cap. At 0.9× the cap the
agent commits; at the cap it halts. An overrun surfaces to the user as
an extension decision.

## §8 — Bbnf-lang specific axes for S-P2

1. **Strict-vs-strict comparator discipline.** Per the
   `beat-lightningcss-target` posture and the SK-V6 finding (sonic-rs
   `utf8_lossy`), P2-A grounds every comparator delta on the strict
   plane. A lossy/permissive comparator row is a flaw probe, never a
   SOTA-beat anchor. P2-A names the strictness plane of every
   comparator path it tears down.
2. **The scalar-oracle-first discipline.** Per `inspect-generated-output`
   and the dav1d process P2-B documents, every SIMD/ASM primitive has a
   scalar reference *first*; the SIMD form is a checkasm-differential
   against it. P2-B produces the admission process; S-P3's
   falsifiability gates enforce it.
3. **The two-layer vocabulary.** Per `general-infra-crates`, bbnf-simd
   carries a Layer-0 vendored substrate and a Layer-1 bbnf primitive
   vocabulary. P2-E places every primitive gap in that two-layer scheme;
   a primitive that belongs in Layer 0 (vendored) is not re-authored.
4. **No god module.** Per `no-god-modules` + `module-structure-codegen`,
   any new primitive is a proper sub-module of bbnf-simd, not a dump
   into a `utils`/`common` kitchen sink.
5. **Grammar lives in the grammar.** Per `hybrid-grammar-host` +
   `regex-generalized`, a candidate that needs JSON structural roles is
   re-framed: either a grammar-neutral primitive consumed by a
   per-grammar template, or a host function. JSON policy never enters a
   generic crate.
6. **The substrate union is Lock 1.** P2-D concludes whether the tape +
   structural projection are one substrate; a candidate that splits
   them, or adds a sidecar event vector, violates Lock 1 and S-P3 may
   not shortlist it.

## §9 — Closing posture

S-P2 is the research pass. It grounds the SK-V{N} engine against the
state of the art and turns each S-P1 hot leaf into a candidate
primitive — scalar-referenced, checkasm-disciplined, grammar-neutral by
construction. It selects nothing; it sequences nothing; it produces the
pool from which S-P3 builds the wave plan.

No S-P3 dispatch without S-P2 convergence. No candidate without a P1
hot-leaf antecedent. No primitive without a scalar reference. No
SOTA-beat against a permissive comparator. No JSON policy in a generic
crate. No re-opened REDRESS route.

The work is bounded by the bench. The design is bounded by the locks.
The candidates are bounded by the profile.
