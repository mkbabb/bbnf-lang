# Retrospective — Tranche AU

Sources: `AU.md`, `PROGRESS.md`, `FINAL.md`, `profiling-{1,2}.md`,
`typed-parity-audit.md`, AT carry-over, AV opening scope, git
`b5d6e1e..3b8b757`.

## 1. Scope vs reality

AU declared 7 phases, 7 waves, 24 gates. Delivered: 10 MET, 2
MET\*, 5 PARTIAL, 5 MISSED, 1 DEFERRED, 1 N/A. Every miss routes
to AV with a named fix sketch. The architectural refactor *did*
ship — unified arena, `ParsedGrammar` deleted (11-tranche backlog),
`.map(|_|())` zeroed, structural bitmap v2, fingerprint-driven
capacity. Misses are bench-perf or codegen-gap, not abandoned.

## 2. Silent vs declared deferrals

One silent: `AU.2.6` colour aggregates were W2 scope but the ≥33 B
arena widening was never attempted; `FINAL.md §1 Phase 2` admits
post-hoc. Bug 1/2/2b surfaced and documented by W6.C parity audit
— discoveries, not silent deferrals. Everything else named at the
wave boundary it slipped.

## 3. Orchestration — samply wave was the template moment

Session 2 is archetypal. Three tooling failures hit
(*prepare-profile-wave* one-entry-only; ripgrep-vs-grep silent
timeouts; bencher substring contamination); two resolved fully
pre-dispatch, one documented-known. Result: 27 (bench, entry)
pairs profiled cleanly on one shared `CARGO_TARGET_DIR`,
wave.tsv consumed verbatim, every claim artefact-cited. Exactly
`PROFILING.md`'s shared-target + file-first discipline at scale.

## 4. Agent-layer friction

Three file-collisions, all resolved at cherry-pick:
W2.B/W2.D on `payload/layout.rs`; W6.A/W6.B on `host.rs`
(`GrammarSink` + `@debug *` merged); W6.B/W6.D double-regen of CSS
tape-parity goldens. Claim-hardening caught two artefacts: the
pre-AU "zero fused CSS scanners" was invalidated by W2.A's
expand; the 7/8 ident misclassification was phantom. Both
corrected in PROGRESS without shipping a non-fix.

## 5. Edict adherence

High. File-first grep over saved expand/bench/profile throughout.
Shared-target held across profiling. Worktrees clean — no `/tmp`,
no trammelling. One slip: `cargo test --workspace` exits 101 on a
pre-existing failure; `--no-fail-fast` reveals 33 hidden by
`stop-at-first`. `FINAL.md §2 gate 17` reports honestly, triages
by origin.

## 6. Chronic deferrals

**In (resolved):** `ParsedGrammar` (11), `StructRegistry` (2),
64-byte padding (3), fresh samply (1), `.map(|_|())`, schema
stubs. **Out (forward-routed with evidence):** Bug 1/2/2b, colour
aggregates, Pratt lowering, Eisel-Lemire, PHF classifier, SoA
substrate. W4's 1.94× prototype proves AoS+arena isn't terminal
but naive SoA doesn't pay — 4-lane unrolling at 6.64× is the
missing lever.

## 7. Mid-tranche restructuring

One genuine restructure: `AU.md §Phase 2` was rewritten in
Session 2 from "CSS scanner activation" to "CSS typed-AST parity
with lightningcss" after `profiling-1.md` made the codegen-gap
framing undeniable. Phase 2 gained AU.2.0 (grammar audit), AU.2.5
(typed dimensions), AU.2.6 (typed colours). Re-planning with more
agents, not scope-creep.

## 8. Template-worthy process artefacts

1. **Seven-wave schedule as literal deliverable** — parallel agents
   with exclusive file-bounds per wave, serial gate-waves (W4, W5,
   W7). Every PROGRESS entry cites its wave. `TRANCHE_SPEC` should
   require this.
2. **Profiling doc as first-class plan artefact** — 27-entry cold
   data tied to reproducible `wave.tsv`; `FINAL.md §5` cites it as
   AV seed. Any tranche with non-trivial perf gates produces a
   same-format `profiling-N.md`.
3. **Honest test reporting with `--no-fail-fast`** at close-out
   distinguishes default-exit from real pass/fail landscape,
   triages each hidden failure by origin.

## Two load-bearing findings

- **Scope-reveal managed by re-planning, not deferring.** Session
  2's Phase-2 rewrite is the correct response to contact with
  profiling evidence, executed without breaking wave cadence.
  `TRANCHE_SPEC` should formalise *re-plan-with-more-agents* as
  the default scope-reveal response.
- **The samply trio — shared `CARGO_TARGET_DIR`, `wave.tsv`,
  headless script — is the orchestration unit that worked.** Three
  pre-wave friction fixes unlocked a 27-entry fan-out every
  downstream doc cites. The trio is the template.
