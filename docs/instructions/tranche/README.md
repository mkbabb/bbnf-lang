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
- [WAVE_SPEC.md](WAVE_SPEC.md) — Per-wave sub-document format,
  including the required per-wave status line.
  Required when a tranche has ≥ 6 waves OR ≥ 4 parallel agents in any
  wave; then each wave carries its own `waves/W<N>.md` spec.
- [AGENT_BRIEF_TEMPLATE.md](AGENT_BRIEF_TEMPLATE.md) — Sub-agent
  prompt boilerplate. Orchestrator substitutes bracketed fields per
  wave; each per-wave dispatch shrinks by ~50% vs re-derived prose.
  `SPEC.md` also permits a narrow prelude annex when build/bench drag
  would otherwise block the tranche's next real wave.

## Invocation order

1. **Close predecessor**. Author predecessor tranche's `FINAL.md` +
   `post-{LETTER}.json` per `SPEC.md` §Closing ceremony.
2. **Research wave** (conditional). Dispatch 3–6 parallel research
   agents per `RESEARCH.md` when the design space is open-ended.
   Artefacts land in `docs/tranches/{LETTER}/research/`.
3. **Author plan**. Write `{LETTER}.md` per `SPEC.md` §Plan structure.
   If a bounded prelude annex is truly necessary, author it first and
   make the blocked tranche name it explicitly.
4. **Per-wave specs** (conditional). If the tranche scope triggers
   `WAVE_SPEC.md` §"When required": author `waves/W<N>.md` per wave
   before dispatching any agent into that wave.
5. **Execute**. Begin per `START.md`.

## File-layout canon

```
docs/tranches/{LETTER}/          # single-pass tranche
├── {LETTER}.md                  # Plan; required. Written before execution; wave table carries status.
├── PROGRESS.md                  # Dated execution log; updated per wave.
├── FINAL.md                     # Closing document; required at tranche close.
├── research/                    # Research-wave deliverables (conditional).
│   └── NN-topic.md
├── audit/                       # In-flight audits; retros.
│   └── *-retro.md
└── waves/                       # Per-wave specs (conditional per WAVE_SPEC); each carries a status line.
    └── W<N>.md
```

For multi-pass tranches per `SPEC.md` §Multi-pass tranche split, each
pass lives in its own suffixed directory with the same internal
structure:

```
docs/tranches/{LETTER}-I/        # pass I
├── {LETTER}-I.md
├── PROGRESS.md
├── FINAL.md
├── audit/
└── waves/
docs/tranches/{LETTER}-II/       # pass II
├── {LETTER}-II.md
├── PROGRESS.md
├── FINAL.md
├── audit/                       # audits informing pass II live here
└── waves/
docs/tranches/{LETTER}-III/      # pass III (unbounded successor passes)
…
```

Benchmarks live at `docs/benchmarks/post-{LETTER}.json` (aggregate) +
`post-{LETTER}-W<N>-{mid,close}.json` per wave; multi-pass variants
use `post-{LETTER}-I.json`, `post-{LETTER}-II.json`, etc., so every
pass carries its own close-matrix artefact.
