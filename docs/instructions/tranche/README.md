# Tranche Authoring Documents

Normative documents for tranche authoring and execution. Composes
`../README.md` (operational directives) and `../PROFILING.md`
(profiling workflow).

## Documents

- [SPEC.md](SPEC.md) — Tranche creation specification. Required
  reading before authoring any new tranche plan. Grounded in the
  12-tranche retrospective corpus (AK–AV).
- [START.md](START.md) — Tranche invocation prompt. The orchestrator's
  entry point; paste to begin a tranche.
- [RESEARCH.md](RESEARCH.md) — Research wave protocol. Six-agent
  fan-out pattern for open-ended design spaces. Run before
  plan-authoring when the design space needs exploration.
- [WAVE_SPEC.md](WAVE_SPEC.md) — Per-wave sub-document format.
  Required when a tranche has ≥ 6 waves OR ≥ 4 parallel agents in any
  wave; then each wave carries its own `waves/W<N>.md` spec.
- [AGENT_BRIEF_TEMPLATE.md](AGENT_BRIEF_TEMPLATE.md) — Sub-agent
  prompt boilerplate. Orchestrator substitutes bracketed fields per
  wave; each per-wave dispatch shrinks by ~50% vs re-derived prose.

## Invocation order

1. **Close predecessor**. Author predecessor tranche's `FINAL.md` +
   `post-{LETTER}.json` per `SPEC.md` §Closing ceremony.
2. **Research wave** (conditional). Dispatch 3–6 parallel research
   agents per `RESEARCH.md` when the design space is open-ended.
   Artefacts land in `docs/tranches/{LETTER}/research/`.
3. **Author plan**. Write `{LETTER}.md` per `SPEC.md` §Plan structure.
4. **Per-wave specs** (conditional). If the tranche scope triggers
   `WAVE_SPEC.md` §"When required": author `waves/W<N>.md` per wave
   before dispatching any agent into that wave.
5. **Execute**. Begin per `START.md`.

## File-layout canon

```
docs/tranches/{LETTER}/
├── {LETTER}.md          # Plan; required. Written before execution.
├── PROGRESS.md          # Dated execution log; updated per wave.
├── FINAL.md             # Closing document; required at tranche close.
├── research/            # Research-wave deliverables (conditional).
│   └── NN-topic.md
├── audit/               # In-flight audits; retros.
│   └── *-retro.md
└── waves/               # Per-wave specs (conditional per WAVE_SPEC).
    └── W<N>.md
```

Benchmarks live at `docs/benchmarks/post-{LETTER}.json` (aggregate) +
`post-{LETTER}-W<N>-{mid,close}.json` per wave.
