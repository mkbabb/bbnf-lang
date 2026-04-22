# Agent Dispatch Template

Reusable template for orchestrator-dispatched sub-agent prompts.
Substitute `{BRACKETED}` fields per wave; everything else carries
as-is. A sub-agent prompt built from this template + the wave-
specific scope substitutions is self-contained — the agent needs
no conversation history.

## Provenance

Derived from `docs/tranches/AX/audit/R2-instructions-redress.md` §P1.
AX.W0a dispatched seven sub-agents with ~800 words of re-derived
preamble per prompt; the template halves per-dispatch prose by
single-sourcing boilerplate.

## Template

```
You are sub-agent {WAVE}.{AGENT_TAG} for tranche {LETTER}.
{ONE_SENTENCE_WAVE_CONTEXT}. Your job: {ONE_SENTENCE_JOB}.

## Worktree (ABSOLUTE ROOT — all work here)

`{WORKTREE_PATH}`

Never leave that directory. Never touch
`/Users/mkbabb/Programming/bbnf-lang` — that is the orchestrator's
main checkout. `target/` symlinks to main; `data/` is seeded.

## Memory discipline (non-negotiable)

Before every cargo invocation:

    export CARGO_BUILD_JOBS=4

Prefer `cargo {test,check} --profile ax-iter` during iteration —
strips DWARF, ~3× lower peak rustc RSS vs default `dev`. Use
`dev` only when samply attribution is needed.

During iteration, use the tiered test runner rather than
`cargo test --workspace`:

    scripts/test-tier.sh leaf        # ~1 min, leaf crates only
    scripts/test-tier.sh grammar     # ~3-5 min, per-grammar bins
    scripts/test-tier.sh workspace   # ~10-15 min, reserved for close

Never run two cargo invocations concurrently. Never link ≥ 4
derive-Parser sites into one test binary (split per-grammar — see
`crates/core/tests/tape_parity_*.rs` for the pattern).

On session resume or mid-run ambiguity, use:

    scripts/worktree-status.sh
    scripts/kill-all-rust.sh --dry-run

## Read first (required, in order)

1. `docs/instructions/README.md` — operational directives.
2. `docs/instructions/tranche/SPEC.md` — tranche spec, §Hard gates,
   §Runtime-evidence, §Activation-gate, §Scope-reveal.
3. `docs/tranches/{LETTER}/{LETTER}.md` — tranche plan, invariants,
   wave summary table.
4. `docs/tranches/{LETTER}/waves/{WAVE}.md` — your wave spec.
5. {WAVE_SPECIFIC_READS}

{ARCHAEOLOGY_NOTE_IF_ANY}

## Scope — {WAVE}.{AGENT_TAG} only

{SCOPE_BULLETS}

Do NOT touch items outside your sub-phase. Sibling agents own
neighbouring scope.

## File bounds

Allow-list:
{ALLOW_LIST}

Forbidden:
{FORBIDDEN_LIST}

## Hard gate

{GATE_ITEMS — each a runtime-verifiable assertion; cite the
verification tool (samply, `cargo expand`, bench delta, test
name, `nm` symbol presence, `wc -l` delta). Every item must
trace to a verification artefact the orchestrator can re-load —
not a claim.}

## Commit discipline

- Use `/commit` or `git commit` with messages citing
  `{LETTER}.{WAVE}.{AGENT_TAG}`.
- Commit at every natural milestone, not at wave end.
- NEVER commit `crates/core/src/grammar/generated.rs` unless
  your sub-phase owns the regen window. Orchestrator regens
  after cherry-pick when multiple agents' emitter changes
  compose.

Commit message template:

    {type}(path): {one-line summary} ({LETTER}.{WAVE}.{AGENT_TAG})

    {rationale — 3-5 lines. Cite the specific file + function
    changed and the runtime-verifiable outcome.}

    {Evidence — cargo expand slice path, bench delta path,
    nm output path, or test name.}

## Return format (to the orchestrator)

≤ {WORD_CAP} words. Dense technical reporting only. Include:

1. Commit SHAs in order with one-line descriptions.
2. {WAVE_SPECIFIC_DELIVERABLE_SUMMARY}.
3. Hard-gate status table — exit status + artefact path for each
   gate item.
4. Any deviation from this spec with rationale.
5. `git status --short` (must be empty or contain only `target/`
   symlink).

No narrative filler. No "I then ran …" prose. No meta-commentary.

## Non-negotiables

- No stubs, no fallbacks, no feature flags, no `#[ignore]` or
  `#[allow(dead_code)]` introduced to hide incomplete work.
- No walker-fallback in shape-emitter code paths
  (`#dispatcher_ident` at inline positions is a bug post W0a).
- One codegen path. One regex system (HIR). No hybrid.
- Runtime evidence for every claim — grep alone is insufficient
  when the emitted code might be dead.
- Idiomatic, gestalt approach. If the right answer involves a
  structural split, split. Don't patch around it.
- If scope-reveal surfaces under contact, halt and report per
  SPEC.md §Scope-reveal — do not silently ship a partial fix.

Begin.
```

## Substitution conventions

| Field | Example |
|---|---|
| `{WAVE}` | `W0a.2.i` or `W3` |
| `{AGENT_TAG}` | `a`, `b`, `c` — lowercase letter per wave sub-agent |
| `{LETTER}` | Tranche letter: `AX` |
| `{WORKTREE_PATH}` | `/Users/mkbabb/Programming/bbnf-wt-ax-w0a-2i-a` |
| `{ONE_SENTENCE_WAVE_CONTEXT}` | "W0a.2.h closed admission-widen; three downstream consumers need rewiring." |
| `{ONE_SENTENCE_JOB}` | "Replace `walk_tape`'s `find_descendant_by_kind` with a shape-emission-compatible cursor." |
| `{WAVE_SPECIFIC_READS}` | Additional paths the agent must read — the specific files it edits, related diag docs, reference implementations. Keep ≤ 6. |
| `{ARCHAEOLOGY_NOTE_IF_ANY}` | If revisiting prior-attempted mechanism per WAVE_SPEC.md §9. Cite prior commits + failure mode + new guardrail. |
| `{SCOPE_BULLETS}` | Numbered scope items. Concrete file + function targets. No speculative items. No "if time allows." |
| `{ALLOW_LIST}` | Bulleted paths the agent may modify. Each cites what it modifies ("`foo.rs` — narrow `bar()` predicate at line N"). |
| `{FORBIDDEN_LIST}` | Paths explicitly out of bounds — sibling agents' scope, unrelated waves, forbidden-by-SPEC categories (`dta_walker/` if W0b-owned, generated.rs if not regen window). |
| `{GATE_ITEMS}` | Per-item: what closes the gate + verification tool. See SPEC.md §Hard gates. |
| `{WORD_CAP}` | 300 for surgical; 500 for moderate; 700 for deep. |
| `{WAVE_SPECIFIC_DELIVERABLE_SUMMARY}` | What the agent reports beyond commits — diagnostic tables, before/after diffs, bench numbers, probe outputs. |

## Invariants inherited across waves (do NOT re-state per-prompt)

These are covered by `docs/instructions/README.md`; the agent
reads them on first step. Including them in every prompt bloats
context:

- Tranche structure + directory layout.
- Crate ownership (full write access; external `path = ../*`
  crates included).
- Commit-at-milestone cadence.
- Worktree isolation rules.
- Bench contract (cold per-parse; mimalloc global allocator).
- Wire-contract pipelines require end-to-end tests.

## Prompts that exceed the template

If a sub-agent's scope genuinely requires > 700 words of
instruction, the wave is mis-scoped — split into multiple
sub-agents on disjoint bounds per SPEC.md §Scope-reveal
§Parallel-probe. The template is the upper bound on sub-agent
prompt complexity; exceed it only under named exception cited
in the parent wave spec.

## Template usage — minimal example

A sub-agent prompt built from the template + per-wave
substitutions typically totals 400-600 words (vs the ~1500-2500
words pre-template). The orchestrator substitutes
`{BRACKETED}` fields, inlines the result, and dispatches.
