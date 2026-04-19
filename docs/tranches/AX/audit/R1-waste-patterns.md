# R1 — Waste-pattern Archaeology (AX Retrospective)

## Summary

Dominant waste pattern: **serial scope-reveal recursion without
root-cause pivot.** AX.W0a.2 absorbed seven sub-waves (`.a`..`.h`)
rediscovering that shape emission is walker-parity-bound and walker
record shape is the de facto truth. Each sub-wave landed narrow fixes
(Array split, AltDispatch, inline-attr downgrade, Keyword Ref-led
Alt, etc.), reverted admission, and handed a ~180-line diag to the
next sub-agent restating the same decomposition. Headline: ~47 AX
commits (`2faedca5..HEAD`) produced zero runtime activation of the
stipulated W0a hard gate, against a plan scoping "2-serial agents."

## Q1. Re-diagnosis loops

**Unclassified-Ref enumeration — 4 independent probes.** `517be13c`
(W0a.2.b) BFS → `post-AX-W0a2b-refs.md`. `2cdb21b7` (W0a.2.f)
`ax_w0a2f_fallback_probe` tagged by ShapeTag. `863d629d` (W0a.2.f)
`ax_w0a2f_extract_probe` dumps same entry-rule body trees. `20ea4c47`
(W0a.2.b) condensed `ax_w0a2b_probe`. Archived at `88f830ac`
(W0a.2.g), re-landed at `67257495` (W0a.2.h). All four share one
observation: post-AltDispatch, 7/7 grammars reach 0 unclassified
Refs.

**Walker tape-parity audits — 4 re-derivations.** W0a.2.d diag lines
104-125 identifies PSI + frame-stack + `pending_variant_idx` as the
gap. W0a.2.e progress lines 55-77 re-walks, re-identifies
`cols_len_after_push`. W0a.2.f §Remaining-blockers lines 142-190
re-walks variant-idx on BNF + `has_iter_ow`. W0a.2.g §Detailed-
blocker-diagnosis lines 49-115 re-catalogs five deltas. None
delegates to a canonical contract doc.

**LLVM inline cycle — 3 diagnoses.** W0a.2.d revert (26 GB RSS
abort). W0a.2.e progress lines 31-52 re-diagnoses as the `parse_array
→ parse_wrap → __value → parse_array` SIGBUS cycle. Fixed at W0a.2.f
`9ffe50db` — four commits, three sub-waves to demote 8 compound shape
fns.

**Bootstrap stub — 3 surfaces.** 23-line stub cited in W0a.2.d lines
56-62, W0a.2.e lines 22-26, W0a.2.f lines 54-62. Same symptom, same
README §Self-host recipe, three re-diagnoses before root cause
(Keyword rejecting Ref-led Alt) landed at W0a.2.g `63845c68`.

**Gate-predicate audits — 6 touches.** `has_w4_classified` /
`has_shape_dispatcher_entrypoint` modified at `9f8aed90` (W0a.1),
`9b1b54e2` (W0a.2), `17617483` (W0a.2.b), `f6e1ecb5`+revert
`63895dee` (W0a.2.f), `10dc846c` (W0a.2.g), `29bfd055` (W0a.2.h).

**Generated.rs sizes.** Five snapshots (96,438 → 96,886) each re-
verifying regen idempotency identically.

## Q2. Wasted commits

Substrate reverted or architecturally subsumed without runtime
activation:

- `f6e1ecb5` (W0a.2.f) `retire body_has_dispatcher_fallback_position`
  → reverted by `63895dee` same sub-wave.
- `4fc87835` (W0a.2.f) `emit_element_position_tape` — shipped,
  admission reverted; dormant code carried to W0a.2.g.
- `2cdb21b7`, `863d629d` probe binaries — archived at `88f830ac`
  one wave later.
- `4f61735f`, `35042584` — two shape-golden regens, one per inline-
  attr wave.
- `517be13c`, `20ea4c47` — W0a.2.b probes re-landed as `ax_w0a2f_*`
  at W0a.2.f; identical behaviour.
- `1e603586` (W0a.2.d) `shapes/inline.rs` substrate (1239 lines).
  Per W0a2d diag §Status, *only the substrate* landed; consumer
  wiring reverted. This is the SYNTHESIS §1 substrate-without-
  consumer anti-pattern, AX day one.

## Q3. Expensive commands per agent

**Bootstrap regens.** W0a.1: 1. W0a.2: 1. W0a.2.d: 2. W0a.2.e: **5**
(W0a2e progress line 155). W0a.2.f: 2. W0a.2.g: 2. **≥ 14 cycles** at
5-10 min each across W0a.2 alone.

**`cargo test --workspace`.** 26 GB RSS rustc pre-OOM on `tape_parity`
aggregate at W0a.2.d; `cargo clean` reclaimed 17.4 GB (W0a2e line
123). W0a.2.e `8048fb41` split the aggregate into six per-grammar
binaries — a build-infra-first fix the `feedback_build_infra_first`
memory predicts should land *before* consuming iteration time. Came
AFTER the OOM.

**`cargo expand`.** 10 artefacts (926 lines total) plus W0a.2.f BBNF
`tape_parity` expand at 91,524 lines. Each expand = 2-5 min per
binary.

**Redundant `cargo check --workspace`.** At every close. Nightly ICE
workaround (`cargo clean -p bbnf-analysis`) ran ≥ 3 times.

**Eliminable via per-crate targeting.** W0a.2.e tape_parity split is
the template (11-14s under `CARGO_BUILD_JOBS=4`). Landing it as first
W0a.2.d deliverable would have saved the OOM + re-dispatch cycle.

## Q4. Agent-briefing overhead

`waves/W0a.md` is 61 lines. Each sub-wave close report is 137-283
lines (avg ~180). Each re-enumerates: (a) the 7×3 predicate table —
appearing verbatim in `post-AX-W0a2f-predicate-table.md`, W0a.2.e
lines 102-114, W0a.2.f lines 89-97, W0a.2.g lines 180-188 (four
identical tables); (b) ShapeTag roster + walker-contract restatement;
(c) rollout-status table.

WAVE_SPEC.md line 159 defines standing context (README, SPEC, parent
letter, wave spec). What's missing: a tranche-local
`WALKER_CONTRACT.md` capturing compound-shape emission, variant_idx
stamping, PSI interaction, inline-attr rules. Estimate ~40% of each
diag doc = repeat prose.

## Q5. Context-burn patterns

Diag docs cite `/tmp/ax-w0a2*.txt` ≥ 14 times per sub-wave (W0a2e
lines 153-162 → 6 files; W0a2f lines 218-248 → 11). Largest cost:
grep/read on `/tmp/expand-bbnf*.txt` (91,524 lines).

`shapes/mod.rs:331` re-read in W0a.2 lines 27-38, W0a.2.d line 9,
W0a.2.e line 95, W0a.2.f line 24, W0a.2.g §1.1 — five re-reads of
~40 LoC. `shapes/inline.rs::emit_inline_position_tape` re-elaborated
in three sub-wave diags.

## Q6. Emergent vs predicted scope-reveals

W0a opened as "2-serial" per `waves/W0a.md:4`; actual seven sub-
waves.

- **W0a.2 reveal.** `shapes/array.rs:105` hard-coded `Some(b'[')` —
  **knowable at plan time** via 5-line inspection. W0a.md authorship
  (`b2fbf095`) drew from `V-audit-overfit.md` without reading the
  array emitter body.
- **W0a.2.d reveal.** 26 GB RSS aggregate LLVM codegen —
  **surfaceable only under contact**. 23-line stub walker-parity gap
  — **knowable at plan time** vs walker's `emit_alt_linear_arm:1387`.
- **W0a.2.e reveal.** `#[inline(always)]` cycle — **partially
  knowable**: pre-existing in master generated.rs.
- **W0a.2.f + W0a.2.g reveals.** Keyword Ref-led Alt + five walker-
  parity deltas (inline Alt compound, Flat Repeat column leaks,
  Repeat(Seq) double-Seq wrap, variant_idx stamping, `has_iter_ow`).
  **Walker-parity chasing** — retrospectively unnecessary per
  W0a.2.h `29bfd055` "shape-authoritative pivot."

**Split: ~30% fundamentally-latent, ~70% avoidable-chase (walker-
parity deltas that W0b's walker deletion erases).**

## Q7. Retro cohort's predictions vs AX failures

Five SYNTHESIS/census-predicted patterns recurred:

1. **Substrate-without-activation (SYNTHESIS §1, 11-tranche chronic).**
   `shapes/inline.rs` 1239-line substrate at W0a.2.d — exact pattern.
2. **Predicate widening without wire-contract** (census line 70).
   AX invariants 9 + 16 encoded the lesson. `has_w4_classified`
   widened at W0a.2.b `f39f1ab2`, W0a.2.f `f6e1ecb5` (reverted
   `63895dee`). Invariant 16's "full bench matrix at commit time"
   never ran for these widenings.
3. **Bench omission (SYNTHESIS §9).** AX §Op-posture 1 declared
   mid+close benches; W0a produced zero `post-AX-W0a*.json` files.
   W0a.md hard-gate #1 (`json_monolithic_value` ≥ 0.98× prototype)
   never verified.
4. **Sub-wave proliferation (SYNTHESIS §4).** Invariant 12 + SPEC
   §Scope-reveal line 251 say "mid-tranche pivots open a new letter."
   Eight serial sub-waves continued under W0a — the AR/AS anti-
   example SYNTHESIS named.
5. **"Architectural transposition; throughput next wave" (invariant
   17).** Forbidden by invariant. Every W0a.2.{d,e,f,g} close ends
   with "Re-plan suggestion for W0a.2.{next}" — violation.

AX encoded invariants 9, 12, 16, 17. Failure: **aspirational prose**,
not **enforcement at sub-wave close.** Close ledgers self-report
("Met under narrow predicate") rather than cite bench artefacts.

## Redress proposals

### R1.1. Tranche-local `WALKER_SHAPE_CONTRACT.md`

Evidence: Q1+Q4 → ~800 lines repeat walker-parity prose across
W0a.2.{d-g}; five re-reads of `shapes/mod.rs:331`; three re-reads of
`inline.rs` helpers.

Proposal: author `docs/tranches/AX/audit/WALKER_SHAPE_CONTRACT.md`
enumerating compound-push per `IrState::{Alt, AltLinear,
AltByteDispatch, Seq, Repeat, Ref}` with line-cited walker code;
variant_idx stamping; PSI/frame-stack interaction; inline-attr rules.
Cite in W0a.2.h+ prompts. Savings: ~40% diag length per sub-wave.

### R1.2. Per-crate build infra before widened-admission

Evidence: Q3 → ≥ 14 regens + 26 GB RSS OOM + 17.4 GB reclaim. Q6 →
W0a.2.e tape_parity split `8048fb41` came AFTER OOM.

Proposal: move per-grammar tape_parity split + `CARGO_BUILD_JOBS=4` +
per-test `cargo expand` template into AX.md §Operational-posture as
plan-time prerequisite. Matches `feedback_build_infra_first`.
Savings: 8-12 agent-hours rediagnosis; eliminates OOM path.

### R1.3. Pinned standing-context per sub-wave

Evidence: Q4 → predicate table verbatim four times.

Proposal: W0a.2.h+ sub-agents dispatch with W0a.2.g progress doc as
pinned context; agent updates the same file at close rather than
spawning a new diag. Savings: ~15-20% context budget per wave.

### R1.4. Mechanical invariant enforcement at sub-wave close

Evidence: Q7 → invariants 9, 12, 16, 17 authored-but-unenforced. AX
§Op-posture #1 stipulated close benches; zero W0a matching artefacts.

Proposal: orchestrator sub-wave close requires either
(a) `post-AX-W0a2<letter>-close.json` with 19-entry matrix, OR
(b) plan-declared interim-state escape with named restoration sub-
wave. No narrative "partial close." SPEC §Closing-ceremony applied
at sub-wave boundary. Would have rejected W0a.2.{d,f} close reports
at reveal, triggering SPEC §Scope-reveal new-letter discriminator.
Savings: the 6+ wasted commits in Q2.

### R1.5. W0a.2.h pivot justifies new-letter escalation

Evidence: W0a.2.h `29bfd055` flips discriminator from "walker-parity
binds shape" to "shape-authoritative; walker is deletion target" —
a tranche-identity shift.

Proposal: close W0a under `docs/tranches/AX/FINAL.md` §Deferred
ledger; open shape-authoritative work as AX'.md per SPEC §Scope-
reveal-protocol line 251. SPEC names AR/AS as the anti-example
W0a.2.{a-h} has exceeded. Each further sub-agent dispatches against
a clean plan ("shape is source of truth; walker is deletion target")
rather than W0a.md's "walker-parity-bound" framing.
