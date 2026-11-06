# SK-V23 research/07 — The Modern Precept Codex

Date: 2026-07-09. Scope: W7. Sources walked: (a) `docs/precepts` submodule at HEAD
`63240e6` (heads/main-12-g63240e6); (b) SK-V20 SPEC.md/PRECEPTS-RECAP.md, SK-V21 SPEC.md,
SK-V22 SPEC.md/PASS-ALPHA.md (the process-surgery lineage); (c) SK-V22 SPEC.md §5 (the
profiling standing law); (d) the engineering-laws block carried verbatim from SK-V21 SPEC.md
into SK-V22. Every item below carries `file:line` evidence. HARD CAP 20 min — the codex below
is the load-bearing subset, not an exhaustive line-by-line transcription of all four sources.

## §0 — Submodule contents (enumerated)

`docs/precepts/` holds: `README.md` (core rules + layout index); `instructions/` — `README.md`
(Edicts + Code/Commit Discipline + Gates), `ORCHESTRATION.md` (wave model, worktree isolation,
dispatch contract, integration, triumvirate, build concurrency, status), `STYLE.md`,
`LESSONS-LEARNED.md` (624 lines, dated incident ledger), `TRANCHE-AND-WAVE-SPEC.md`,
`CONSUMING.md`, `style/CALIBRATION.md`, `tranche/{README,SPEC,START,RESEARCH,CHALLENGE,
DOC_UPDATE_WAVE,WAVE_SPEC,AGENT_DISPATCH_TEMPLATE}.md`; `audits/overfitting-audit.md` +
`audits/REAUDIT-2026-04-30/*` (a prior cross-repo hardening pass); `glossary/meta-terms.md`
(732 lines, canonical process vocabulary); `cross-repo-dev-resolution.md` (320 lines, the
multi-repo workspace-resolution contract); `infra/{tls,deploy,blob-backend-dr,domains}.md`
(deployment ops, out of scope for this codex — no process-precept content).

## §1 — The numbered codex

### Core discipline (submodule, applies to both repos' implementation work)

1. KISS/DRY; smallest rule that prevents the real failure. — `README.md:5`
2. No workarounds, stubs, disabled gates, or compatibility shims outside a plan-declared
   bounded brittleness window. — `instructions/README.md:10-12`
3. Architectural transposition over patching; reject sprawling multi-PR sweeps when one
   transposition lands cleaner. — `instructions/README.md:13-16`
4. Abrogate before patch: for intrinsically failing subsystems, ask "can we delete?" before
   "can we patch?". — `instructions/README.md:17-19`
5. One path: two orthogonal codepaths for the same logic collapse to one. — `instructions/README.md:20-22`
6. No legacy code: delete, never rename-and-hide or feature-flag-and-keep. — `instructions/README.md:23-24`
7. No silent deferrals: planned work lands, is formally retired with rationale, or moves to a
   same-tranche named destination; cross-tranche deferral is a scope-reveal trigger. — `instructions/README.md:25-27`
8. Substrate lands with a consumer (runtime caller, test, benchmark, or other proof). — `instructions/README.md:28-29`
9. No overfitting: single-use private helpers inline; unused public surfaces delete. — `instructions/README.md:30-32`
10. Wire-before-retire: an under-wired primitive resolves via wire (≥2 consumers), retire
    with rationale in the prune ledger, or defer to a named next-letter tranche — never
    "future work". — `instructions/README.md:33-49`
11. Every wave is named (`W<N> - <Title>`); bare `W<N>` is invalid once a name exists. — `instructions/README.md:50-54`, `instructions/ORCHESTRATION.md:12-15`
12. Every unit (tranche/wave/sub-wave) declares both a goal criterion and a completion
    criterion, paired but distinct. — `instructions/README.md:55-65`
13. Fail-explicit on library-internal contract violations (throw); browser/environment
    degradation paths may fail silent with rationale — the two modes never collapse. — `instructions/README.md:82-93`
14. No god modules: `utils.rs`/`helpers.rs`/`common.rs` split by concern, never by namespace
    position, at every level (crate/module/file). — `instructions/README.md:105-108`
15. Splits use directory modules (`hir/mod.rs` + children), not flat siblings
    (`hir_leaf.rs`); cross-crate isomorphism over flat naming. — `instructions/README.md:110-113`
16. Test files live outside `src/`; co-located `*.test.ts`/`#[cfg(test)]` pollute the
    audit-walk surface. — `instructions/README.md:126-130`
17. Documentation rides the same wave as the architectural change it describes; doc drift
    blocks wave close. — `instructions/README.md:132-135`
18. Commits: Conventional Commits with concrete module/wave scopes, no arbitrary subject-length
    cap, body required for multi-subsystem/generated/gate-changing/`--no-verify` commits. — `instructions/README.md:137-169`
19. A hard gate is valid only with an artefact (build/test/runtime/bench/generated-diff/
    deletion-proof/plan-required doc update); grep-only checks are supplementary. — `instructions/README.md:171-185`

### Orchestration mechanics (submodule)

20. Hard ceiling: at most 6 agents in a wave (dual ceiling: read-only audit lanes may reach
    7). — `instructions/ORCHESTRATION.md:11,28-34`; reinforced as a rejection rule at integration — `instructions/LESSONS-LEARNED.md:216-224`.
21. Worktree isolation required when ≥2 parallel agents may write shared territory;
    recommended at ≥3 agents. — `instructions/ORCHESTRATION.md:36-49`
22. Agents never commit; the orchestrator integrates via cherry-pick, never merge. — `instructions/ORCHESTRATION.md:52-55,95-99`
23. Cross-repo commits: additive annotation permitted under narrow conditions; authority-
    transfer requires both tranches' open docs to declare bounds; push is always
    orchestrator-authored, never agent-authored. — `instructions/ORCHESTRATION.md:107-138`
24. Empty sub-agent return is a failed dispatch (redispatch verbatim), not scope-reveal;
    triumvirate is mandatory if the redispatch also returns empty. — `instructions/ORCHESTRATION.md:230-235`
25. Triumvirate (research → plan-augment → redress) is the default recovery shape for
    non-environmental stalls; auto-triggers on >15min quiet, empty first-pass, 3 failed
    diagnostic loops, or scope reveal. — `instructions/ORCHESTRATION.md:173-215`
26. Every triumvirate dispatch carries an explicit hard cap (defaults: research 20min,
    plan 15min, redress 30min, audit lanes 25min); "at 0.9N commit, at N halt". — `instructions/ORCHESTRATION.md:195-196`
27. At most one cargo invocation in flight per `CARGO_TARGET_DIR`. — `instructions/ORCHESTRATION.md:217-221`
28. Commands >60s use `run_in_background` + Monitor; never poll via `tail -f`/`ps`/sleep
    loops. — `instructions/ORCHESTRATION.md:223-228`
29. Status-tick cadence: ~5min orchestrator-silent-wait ceiling before a one-line tick. — `instructions/ORCHESTRATION.md:257-264`
30. Reconcile the task census (JSONL mtimes / ps / harness list) before any status reply;
    zombie agents are frequent. — `instructions/ORCHESTRATION.md:266-273`
31. Scope pivots beyond a 2-path/unchanged-gate absorption ceiling open a new tranche letter,
    never continue in-place. — `instructions/LESSONS-LEARNED.md:204-213`

### Process surgery (SK-V20/21/22 lineage — binding on this campaign specifically)

32. One challenge wave per open-ended research pass; advance-on-zero-REJECT (the old §3Z
    escalation ladder is retired in favor of it). — `sk-v20/SPEC.md:33`
33. The astral scoring layer (ΩA-ΩF) is retired; SIGNOFF + CRUD-LOG remain as the surviving
    close artefacts. — `sk-v20/SPEC.md:33`
34. No new planning cycle opens while the chronic ledger (DEFERRED-LEDGER) is non-empty. — `sk-v20/SPEC.md:34`
35. Every wave closes on a green runtime gate in `crates/` — never on a document. — `sk-v20/SPEC.md:34-35,177`
36. Non-implementation (research/audit) passes are capped at three per cycle. — `sk-v20/SPEC.md:35`; reaffirmed `sk-v21/SPEC.md:460`
37. Y.13 consumer-invariant gate law: every substrate/kernel/value wave closes on an
    exhaustive-match runtime gate over its emitted decision surface, liveness AND (absent a
    named, ledgered relaxation) a measured win — not liveness alone. Two relaxations are
    named and closed (TW1b, TW4.1); no further relaxation without a new-letter re-plan. — `sk-v21/SPEC.md:66-84`, `sk-v21/SPEC.md:745-750`
38. Profile-driven planning beats spec-driven: the corpus scores profile-driven tranches 2/2
    (BEAT-or-peak, zero dead substrate) against spec-driven 12/14 dead-substrate instances. — `sk-v21/SPEC.md:87-89`
39. "Complete with recorded misses" is not a close state; a wave that cannot close is
    re-planned under a new letter, never absorbed. — `sk-v21/SPEC.md:90-91`
40. Every substrate lands with its consumer in the same wave (the four-hop pipeline — IR fact
    → cost model → emitter selection → consumer reads the const — closes all four hops or the
    wave does not open). — `sk-v21/SPEC.md:748-750`
41. Batches of three agents in parallel is the campaign-specific fan-out default (tighter
    than the submodule's 6-agent ceiling); Core model orchestrates/synthesizes, Opus/Sonnet
    fan out; critique before every wave close. — `sk-v22/PASS-ALPHA.md:76-79`, `sk-v22/SPEC.md:536`
42. The archive sequence is tags-before-removals, always, order normative. — `sk-v22/SPEC.md:468` (context header, §2.1 cross-ref)

### The profiling standing law (SK-V22 SPEC §5 — cold, ratio-certified, load-accounted)

43. PMU cycles/byte via `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` is the certified column;
    ins/byte is the load-invariant check; compute-bound lanes drift ≤3% across windows,
    memory-bound lanes are flagged. — `sk-v22/SPEC.md:472-475`
44. Paired per-sample interleaving: both sides of a ratio timed back-to-back inside one
    sample, mean + 95% CI over N≥200; decision-grade only when every window's CI excludes 1.0
    in the same direction. — `sk-v22/SPEC.md:476-478`
45. Cold-per-parse only (N=200, one out-of-band touch, fresh state, min AND median reported);
    no figure is ever quoted warm. — `sk-v22/SPEC.md:479-480`
46. Multi-window repetition: ≥3 separated capture windows, cross-window agreement reported. — `sk-v22/SPEC.md:481`
47. Per-block loadavg recorded beside every table; wall ns/byte and Mbps are labeled
    DIRECTIONAL, median is the robust wall statistic; PMU ratios are the certified column,
    absolutes remain directional. — `sk-v22/SPEC.md:482-483`
48. Every wave's no-regression clause binds to the graduation baseline as the standing
    anchor; each wave's own regression proof is a paired same-session before/after capture —
    the anchor never moves, the proof always pairs. — `sk-v22/SPEC.md:487-490`
49. The loadavg<2 "quiet" gate is formally retired as environmentally unachievable and
    replaced wholesale by the load-accounted method above (items 43-48). — `sk-v22/SPEC.md:463-467`

### Engineering laws (carried SK-V21→SK-V22 verbatim, binding on both repos' source)

50. No workarounds, no stubs, no legacy code; architectural transposition over patching;
    prototypes are worktree-isolated analysis instruments, never landed on master. — `sk-v21/SPEC.md:755-756`
51. Benchmarks: cold-only, single-invocation, aarch64/Apple-M only; every wave benches every
    grammar/target — not just the one under change. — `sk-v21/SPEC.md:758-759`
52. One cargo invocation in flight per target dir; profiling via interactive samply with
    `debug=true`; long runs redirect to a file once, then grep. — `sk-v21/SPEC.md:760-761`
53. Generated files are fresh-regen output, never hand-patched; a `generated_loc_budget` is
    enforced per tranche; the idempotent-on-disk rule holds (a generator re-run against
    unchanged input produces a byte-identical file, so `include!`-consumed source is skipped
    when unchanged). — `sk-v21/SPEC.md:763-764`, `sk-v21/SPEC.md:518`
54. Tests live in `tests/`, never `#[cfg(test)]` in `src/`. — `sk-v21/SPEC.md:764`
55. Doc updates ride their code commits in the same wave; standalone prose, no
    meta-language (no plan/commit/conversation references in product docs). — `sk-v21/SPEC.md:764`
56. Fail-explicit / no-silent-fallback on internal contract violations generalizes to
    codegen lowering: unknown rule_kinds panic rather than silently emitting Epsilon (a
    bbnf-lang-specific instantiation of item 13/56; not textually present in the submodule,
    evidenced instead at `sk-v20/SPEC.md:87` — "no silent no-ops" for
    SpanMark/DirectBuild/RegexProgram-kind/AltMode::Dispatch, and by the codebase's own
    `feedback_no_silent_epsilon` convention).
57. Clean instrumentation: `eprintln!` soft-fails are hard asserts, not swallowed. — `sk-v20/SPEC.md:62`

## §2 — Contradictions found

**C1 — Agent fan-out ceiling, three conflicting numbers.** The submodule's hard ceiling is 6
(exceeding it is an integration-time rejection rule) — `instructions/ORCHESTRATION.md:11`,
`instructions/LESSONS-LEARNED.md:216-224`. SK-V22's campaign default is 3 —
`sk-v22/SPEC.md:536`. SK-V23's own charter, in the very pass protocol this codex is produced
under, specifies "up to 8 agents in parallel research" — `sk-v23/PASS-ALPHA.md:48` — which
exceeds the submodule's hard ceiling by 2 and is not one of the submodule's two carved
exceptions (read-only audit lanes, capped at 7). This is a live, unresolved tension the W7
codex surfaces rather than silently resolves: SK-V23's W1 (or its dispatching orchestrator)
should either cite the read-only-audit-lane exception explicitly for the 8-agent research
fan-out, or reduce to 6 (submodule ceiling) or 3 (campaign default), before the next fan-out
dispatches.

**C2 — "Never on a document" vs the submodule's own gate-artefact list.** SK-V20's process
surgery states every wave closes on a runtime gate "never on a document" —
`sk-v20/SPEC.md:34-35,177`. The submodule's generic Gates list explicitly includes "explicit
document update required by the plan" as a valid hard-gate artefact type —
`instructions/README.md:171-185` (item 19 above). These are reconcilable (SK-V20's rule binds
specifically to substrate/kernel/value waves — Y.13's own wave-class carve-out at
`sk-v21/SPEC.md:75-76` names doc-closing waves — TW0, TW-S, TW5, TW6 — as closing on their own
named gate class, which may be a doc update), but a plan reader applying only the SK-V20
one-liner without the Y.13 carve-out will over-apply "never a document" to hygiene/doc waves
that the same lineage explicitly permits to close on documents.

**C3 — Batches-of-three vs six-agent ceiling is not a contradiction but an unexplained
narrowing.** No source states why SK-V22 tightened 6→3; absent a stated rationale this reads
as an undocumented campaign-local override rather than a considered precept, and SK-V23's
codex should either inherit 3 explicitly or state why it reverts toward 6-8.

No other cross-source contradictions surfaced under the 20-minute cap; the process-surgery
items (32-42) are additive refinements of the submodule's generic rules (19-31), not
conflicts — e.g. Y.13 (37) sharpens "hard gate is valid only with an artefact" (19) rather
than replacing it, and "no planning cycle opens while chronic ledger is non-empty" (34) is a
stricter instance of "no silent deferrals" (7).

## §3 — Applicability: parse-that tracks vs bbnf tracks

**Binds both repos, unconditionally (per PASS-ALPHA's own three axes,
`sk-v23/PASS-ALPHA.md:9-23`):** items 1-19 (core discipline), 50-57 (engineering laws), and
43-49 (the profiling standing law) — any track that ships a performance claim (bbnf's
JSON/CSS kernels, parse-that's regex-engine SOTA absorptions PT-R.3, both repos' NEON
modernization under axis 2) is bound to the same PMU/paired-interleaving/cold-only discipline
by the shared campaign, not by repo-specific carve-out.

**Binds bbnf-lang directly, extends to parse-that by campaign framing, not by native
authority:** items 32-42 (the SK-V20/21/22 process-surgery items, including Y.13) are bbnf-
lang tranche-lineage law. SK-V22 SPEC frames PT-R/PT-T as riding alongside GW0-GW6 under "the
Y.13 law" (`sk-v22/SPEC.md:129,567`), but PT-R/PT-T are parse-that's OWN tranche letters
(`sk-v23/PASS-ALPHA.md:44`, avoiding collision with its native A/B/Q/S series) — SK-V23's
codex governs what those tranches should CONTAIN, not how parse-that's own sessions execute
and commit them. The non-clobber law (`sk-v23/PASS-ALPHA.md:35-44`) is dispositive here:
nothing in parse-that is modified, staged, or committed by this campaign; PT-R/PT-T land under
parse-that's own orchestration discipline, which may diverge from bbnf's (e.g. its own
six-agent-vs-batches-of-three answer, its own commit-scope conventions) and this codex cannot
presume otherwise absent parse-that's own precepts submodule pull.

**Binds orchestration mechanics (20-31) to whoever is dispatching agents in either repo** —
worktree isolation, cherry-pick-not-merge, triumvirate, build concurrency, status-tick
cadence are host-agnostic operating discipline for any orchestrator running this precept set,
independent of which repo's source is under change.

**Cross-repo-specific:** item 23 (cross-repo commit policy) and the non-clobber law govern
the SEAM between the two repos specifically — additive-annotation and authority-transfer are
the only two sanctioned modes for bbnf-side agents to touch parse-that, and SK-V23's own
charter narrows this further to zero (one additive coordination letter only, at close).
