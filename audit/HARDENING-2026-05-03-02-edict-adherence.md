# Hardening 2026-05-03 02 — Edict-Adherence

Lane: 02 Edict-Adherence.
Scope: `docs/tranches/BA`, `docs/tranches/BB`, `docs/tranches/BC` top-level plans and active `waves/W0..W6.md`.
Method exemplar: `docs/tranches/meta-audit/02-instruction-adherence.md`.
Output boundary: audit report only. No source or tranche-spec edits were made.

## Verdict

The BA/BB/BC runway is broadly edict-aware: all 21 wave specs are named, all declare agent counts at or below six, all contain Triumvirate Dispatch and Worktree Plan sections, and the top-level plans preserve deletion bias, named cross-tranche destinations, and evidence-backed hard gates.

Three violations require amendment before BA.W0 opens:

1. **BB.W0 rank/tier skeletons are substrate-forward with W3-deferred consumers.**
2. **BA top-level authorises HARD CAP extension after overrun.**
3. **Generated-output size budgets are absent from the waves that change generators and generated outputs.**

Two partials should be tightened:

1. **Status-tick cadence is not encoded in the audit-subject orchestration rules.**
2. **Per-wave doc reconciliation is explicit at W6, but not consistently named in W0-W5 commit plans.**

No new memory proposal emerged. The failures are applications of existing precepts: `feedback_no_workarounds`, `feedback_single_cargo_per_target`, `feedback_generated_size_budget`, and the existing HARD CAP / substrate-with-consumer rules.

## Per-Edict Table

| ID | Edict quote | Subject evidence | Verdict |
|---|---|---|---|
| E01 | "`KISS. DRY.` Use the simplest complete mechanism. Remove duplication before adding policy." (`docs/precepts/instructions/README.md:8`) | BA frames "NO quick solutions... KISS, ONE PATH" and names deletion targets (`docs/tranches/BA/BA.md:40`). BB forbids backward compatibility and stubs (`docs/tranches/BB/BB.md:18`). BC says cleanup retires residue and invents no new architecture (`docs/tranches/BC/BC.md:25`). | ADHERES |
| E02 | "`No quick fixes.` Workarounds, stubs, disabled gates, and compatibility shims are debt unless the plan explicitly declares a bounded brittleness window and restoration wave." (`docs/precepts/instructions/README.md:10-12`) | BA declares no tranche-wide brittleness window (`docs/tranches/BA/BA.md:203`) and deletes legacy surfaces (`docs/tranches/BA/BA.md:142-146`). BB.W0 violates via "skeleton" and "stub results" deferred to W3 (`docs/tranches/BB/waves/W0.md:78-82`). BA also allows TS RED closure by `#[ignore]` with named successor (`docs/tranches/BA/BA.md:136`), which is acceptable only if treated as TS/WASM punt debt, not a BA gate closure. | VIOLATION |
| E03 | "`Abrogate before patch.` For intrinsically failing subsystems, ask \"can we delete?\" before \"can we patch?\"." (`docs/precepts/instructions/README.md:17-19`) | BA.W0 deletes dead module clusters and prefers `merge_path_seed` deletion unless BB.W1 consumes it (`docs/tranches/BA/waves/W0.md:14-15`). BA deletion bias forbids compensating substrates (`docs/tranches/BA/BA.md:160-178`). BB and BC carry deletion-bias sections (`docs/tranches/BB/BB.md:129-138`, `docs/tranches/BC/BC.md:126-136`). | ADHERES |
| E04 | "`One path.` Two orthogonal codepaths for the same logic is a code smell, not a feature flag." (`docs/precepts/instructions/README.md:20-22`) | BA.W4 collapses eager `parse(input)` into `parse_with(input, &EMPTY_PATH)` and retires `__EAGER_EMPTY_PATH` (`docs/tranches/BA/waves/W4.md:88-107`). BA.W5 retires `LegacyPath`, `cursor.match_*`, and per-grammar `__path_plan` (`docs/tranches/BA/BA.md:144-146`). | ADHERES |
| E05 | "`No legacy code.` Delete dead code. Do not rename it, hide it behind a feature flag, or leave commented remnants." (`docs/precepts/instructions/README.md:23-24`) | BA top-level lists the legacy surfaces as wave-owned deletion targets (`docs/tranches/BA/BA.md:40`). BC.W1 deletes the oversized flat `runtime/css_l4/builder.rs` after directory split (`docs/tranches/BC/waves/W1.md:34`, `docs/tranches/BC/waves/W1.md:93`). | ADHERES |
| E06 | "`No silent deferrals.` Planned work lands, is formally retired with rationale, or moves to a same-tranche named destination. Cross-tranche deferral is a scope-reveal trigger, not a routine close path." (`docs/precepts/instructions/README.md:25-27`) | BA names non-routable carries and named cross-tranche debt (`docs/tranches/BA/BA.md:63-86`, `docs/tranches/BA/BA.md:180-185`). BB and BC do the same (`docs/tranches/BB/BB.md:55-78`, `docs/tranches/BC/BC.md:58-78`). TS/WASM routes to BD by name (`docs/tranches/BC/BC.md:56`, `docs/tranches/BC/BC.md:122`). | ADHERES |
| E07 | "`Substrate with consumer.` New abstractions land with a runtime caller, test, benchmark, or other proof that the abstraction is consumed." (`docs/precepts/instructions/README.md:28-29`) | BA states same-wave consumption (`docs/tranches/BA/BA.md:41`). BC states substrate is consumed in the same wave (`docs/tranches/BC/BC.md:25`). BB.W0 violates by landing rank/tier skeletons whose implementations and consumers land in W3, with whitelist recognition of a deferred consumer (`docs/tranches/BB/waves/W0.md:80-82`). | VIOLATION |
| E08 | "`No overfitting.` ... must have a current consumer and evidence. Single-use private helpers inline. Unused public surfaces delete." (`docs/precepts/instructions/README.md:30-32`) | Substrate-audit is a close gate in BA, BB, and BC (`docs/tranches/BA/BA.md:152`, `docs/tranches/BB/BB.md:123`, `docs/tranches/BC/BC.md:119`). Same BB.W0 skeleton exception violates the current-consumer half. | VIOLATION |
| E09 | "`Every wave is named.` Every wave carries both a number and a title..." (`docs/precepts/instructions/README.md:33-36`) | BA, BB, BC wave tables use `W<N> - <Title>` for all waves (`docs/tranches/BA/BA.md:92-98`, `docs/tranches/BB/BB.md:84-90`, `docs/tranches/BC/BC.md:84-90`). Each wave header carries `**Name**: W<N> - <Title>`; sample BA.W2 (`docs/tranches/BA/waves/W2.md:3`). | ADHERES |
| E10 | "`Evidence beats claims.` Agent reports are checked against artefacts." (`docs/precepts/instructions/README.md:38`) and "A hard gate is valid only when it can be verified by an artefact" (`docs/precepts/instructions/README.md:106-114`) | BA forbids API-existence / grep-only / consumer-later gate closure (`docs/tranches/BA/BA.md:42`). BA.W4 saves tests, same-harness JSON, samply summary, and the 7-artefact profile directory (`docs/tranches/BA/waves/W4.md:114-122`). BB.W6 and BC.W6 require FINAL artefacts for every gate (`docs/tranches/BB/waves/W6.md:80-86`, `docs/tranches/BC/waves/W6.md:80-86`). | ADHERES |
| E11 | "Hard ceiling: use at most 6 agents in a wave." (`docs/precepts/instructions/ORCHESTRATION.md:11`) | BA uses 5/6/3 agents, with W2 at the ceiling (`docs/tranches/BA/BA.md:92-98`, `docs/tranches/BA/waves/W2.md:5`). BB and BC use 5 or 3 (`docs/tranches/BB/BB.md:84-90`, `docs/tranches/BC/BC.md:84-90`). | ADHERES |
| E12 | "Default to the smallest agent count that preserves disjoint ownership." (`docs/precepts/instructions/ORCHESTRATION.md:16`) | All active wave specs include disjointness sections and Worktree Plan tables. BA.W2 maps six disjoint agent units to six target dirs (`docs/tranches/BA/waves/W2.md:58-71`). BC.W4 splits if the route-to-W4 list exceeds five items (`docs/tranches/BC/waves/W4.md:23`, `docs/tranches/BC/waves/W4.md:41-43`). | ADHERES |
| E13 | "Assign explicit may-read, may-modify, and must-not-touch paths." (`docs/precepts/instructions/ORCHESTRATION.md:17`) | Wave specs include File Bounds and "Do NOT touch" clauses. BA.W2 lists generated, runtime, tests, benches, and exclusions (`docs/tranches/BA/waves/W2.md:38-56`). BC.W6 excludes source code (`docs/tranches/BC/waves/W6.md:31-44`). | ADHERES |
| E14 | "Do not dispatch implementation agents from a dirty or ambiguous main worktree. At wave open, record `git status --porcelain`, staged paths, and the base commit." (`docs/precepts/instructions/ORCHESTRATION.md:61-63`) | BA top-level requires `git status --short`, staged paths, base commit, worktree list, and target dirs before dispatch (`docs/tranches/BA/BA.md:121`). BB/BC reference the same discipline (`docs/tranches/BB/BB.md:106`, `docs/tranches/BC/BC.md:106`). | ADHERES |
| E15 | "Every triumvirate dispatch carries `HARD CAP: N min. At 0.9N commit, at N halt.`" (`docs/precepts/instructions/ORCHESTRATION.md:117-118`) and "every dispatch prompt carries `HARD CAP... at N halt.`" (`docs/precepts/instructions/LESSONS-LEARNED.md:155-156`) | Every wave spec includes the HARD CAP sentence; sample BA.W2 (`docs/tranches/BA/waves/W2.md:145`). BA top-level then contradicts it: "the orchestrator extends the cap" after overrun (`docs/tranches/BA/BA.md:129`). BB/BC say "Same as BA" (`docs/tranches/BB/BB.md:106`, `docs/tranches/BC/BC.md:106`), so the inheritance is ambiguous. | VIOLATION |
| E16 | "The orchestrator must dispatch a triumvirate when... JSONL quiet >15 min; first-pass no-commit/no-evidence; three diagnostic-loop iterations; scope reveal invalidates file bounds, hard gate, or substrate-with-consumer wiring." (`docs/precepts/instructions/ORCHESTRATION.md:122-131`) | BA top-level repeats the four auto-triggers (`docs/tranches/BA/BA.md:130`). Each active wave has a Triumvirate Dispatch section; BA.W2 includes all four trigger classes (`docs/tranches/BA/waves/W2.md:23-36`). BB.W6 and BC.W6 include substrate-audit and close-honesty triggers (`docs/tranches/BB/waves/W6.md:19-26`, `docs/tranches/BC/waves/W6.md:19-26`). | ADHERES |
| E17 | "Any command expected to run >60 seconds uses `run_in_background` + Monitor. Never poll via `tail -f`, `ps aux`, or sleep loops." (`docs/precepts/instructions/ORCHESTRATION.md:147-149`) | The active wave specs do not restate Monitor/no-poll in each prompt, but `AGENT_DISPATCH_TEMPLATE.md` is the prompt source and contains the command rule (`docs/precepts/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md:54-56`). Top-level plans do not contradict it. | ADHERES |
| E18 | "When sub-agents are running and the orchestrator has been silent to the user for ~5 minutes, emit a one-line status tick..." (`docs/precepts/instructions/ORCHESTRATION.md:181-186`) | BA/BB/BC top-level orchestration rules cover auto-triggers, empty returns, caps, and worktrees but do not mention 5-minute status ticks (`docs/tranches/BA/BA.md:117-131`, `docs/tranches/BB/BB.md:104-106`, `docs/tranches/BC/BC.md:104-106`). | PARTIAL |
| E19 | "An empty sub-agent return is treated as a failed dispatch... redispatch verbatim... if the second dispatch also returns empty, triumvirate is mandatory." (`docs/precepts/instructions/ORCHESTRATION.md:154-157`) | BA top-level and BA.W2 encode the rule (`docs/tranches/BA/BA.md:122`, `docs/tranches/BA/waves/W2.md:34-36`). BB/BC top-level inherit it (`docs/tranches/BB/BB.md:106`, `docs/tranches/BC/BC.md:106`). | ADHERES |
| E20 | "`Status`... At each wave boundary, update: PROGRESS.md; wave spec status line; parent wave table; FINAL.md, when closing..." (`docs/precepts/instructions/ORCHESTRATION.md:168-174`) and "Every wave close includes documentation reconciliation... not optional." (`docs/precepts/instructions/tranche/DOC_UPDATE_WAVE.md:3-4`) | W6 specs explicitly update PROGRESS/FINAL/status evidence (`docs/tranches/BA/waves/W6.md:18-21`, `docs/tranches/BB/waves/W6.md:14-16`, `docs/tranches/BC/waves/W6.md:14-16`). Earlier wave commit plans usually name audit docs but not PROGRESS/status reconciliation; sample BA.W2 commit plan (`docs/tranches/BA/waves/W2.md:141-145`) and BC.W0 commit plan (`docs/tranches/BC/waves/W0.md:111-115`). | PARTIAL |
| E21 | "`At most one cargo invocation per CARGO_TARGET_DIR.` Sibling worktrees set per-agent `CARGO_TARGET_DIR=<worktree>/target/<agent>`." (`docs/precepts/instructions/LESSONS-LEARNED.md:175-176`) | All active wave specs have Worktree Plan tables. BA.W2 enumerates six unique target dirs (`docs/tranches/BA/waves/W2.md:62-71`); BB.W6 and BC.W6 enumerate three each (`docs/tranches/BB/waves/W6.md:50-56`, `docs/tranches/BC/waves/W6.md:50-56`). BA top-level also forbids concurrent cargo against a shared target dir (`docs/tranches/BA/BA.md:127`). | ADHERES |
| E22 | "`No stubs.` / `No forward hooks without same-wave consumers.`" (`docs/precepts/instructions/tranche/WAVE_SPEC.md:153-154`) | BB.W0 says rank/tier skeletons compile but produce stub results, and substrate-audit whitelists the W3-deferred consumer (`docs/tranches/BB/waves/W0.md:78-82`). | VIOLATION |
| E23 | "Every tranche that touches a code generator declares an expected output line-count window for each generated artefact." (`docs/precepts/instructions/LESSONS-LEARNED.md:285-286`) | BA.W2 modifies emitters and generated outputs (`docs/tranches/BA/waves/W2.md:13-16`, `docs/tranches/BA/waves/W2.md:46`) but its artefacts lack a generated-size-budget table (`docs/tranches/BA/waves/W2.md:129-139`). BB.W4 regenerates outputs and tracks shrink (`docs/tranches/BB/waves/W4.md:76-90`) but likewise lacks an expected per-artefact window (`docs/tranches/BB/waves/W4.md:107-113`). | VIOLATION |
| E24 | "Plan prose follows `STYLE.md`... AI-writing-sign avoidance." (`docs/precepts/instructions/tranche/SPEC.md:37-38`) and "Output must be orthogonal to the Wikipedia `Signs of AI writing` catalogue..." (`docs/precepts/instructions/STYLE.md:62-73`) | Targeted scan found no banned STYLE phrases in active BA/BB/BC prose except `leverage` inside a verbatim user quote in BA's TS/WASM punt (`docs/tranches/BA/BA.md:191`), which is quote-preserving rather than plan voice. | ADHERES |
| E25 | "Do not edit the submodule from a consumer tranche unless that tranche's scope explicitly includes changing shared process." (`docs/precepts/instructions/CONSUMING.md:69-72`) | BC.W3 explicitly scopes a `docs/precepts/instructions/PROFILING.md` submodule update and parent pin bump (`docs/tranches/BC/waves/W3.md:68-72`). Other waves do not touch `docs/precepts`. | ADHERES |
| E26 | "Every open-ended research wave is followed by a challenge wave before plan synthesis." (`docs/precepts/instructions/tranche/CHALLENGE.md:3-4`) | BA/BB/BC active waves are implementation/measurement/cleanup waves, not open-ended research waves. Triumvirate research outputs are recovery artefacts, not plan-synthesis research waves (`docs/precepts/instructions/ORCHESTRATION.md:95-118`). | NOT APPLICABLE |
| E27 | "Use a research wave when the design space is open enough that a plan would be guesswork." (`docs/precepts/instructions/tranche/RESEARCH.md:3-4`) | Plans route unclear root-cause and scope reveal to triumvirate rather than inventing implementation scope; sample BA.W2 (`docs/tranches/BA/waves/W2.md:23-36`). No open design-space wave is declared without research/challenge posture. | ADHERES |
| E28 | "Scope bullet text follows `STYLE.md`. Concrete change or deletion phrasing, no filler." (`docs/precepts/instructions/tranche/WAVE_SPEC.md:39-43`) | Scope bullets are concrete across the sampled waves: BA.W2 emits/deletes/regenerates specific paths (`docs/tranches/BA/waves/W2.md:13-21`); BC.W1 splits named oversized files (`docs/tranches/BC/waves/W1.md:11-15`). | ADHERES |
| E29 | "Hard gates close on artefacts. Invalid hard gates: `API exists`; `grep found a source string` for runtime behaviour; `consumer will be wired later`; disabled feature flag with no restoration wave; narrative-only proof." (`docs/precepts/instructions/tranche/SPEC.md:63-78`) | Most gates name commands, benchmarks, or deletion proofs. BB.W0's deferred-consumer substrate-audit whitelist is an invalid "consumer later" proof (`docs/tranches/BB/waves/W0.md:80-82`). BA.W4 uses grep only for structural deletion of `__EAGER_EMPTY_PATH`, and pairs runtime behaviour with tests and nextest (`docs/tranches/BA/waves/W4.md:92-107`). | VIOLATION |

## Findings

### A1. BB.W0 violates substrate-with-consumer and no-stub edicts

Edict:

> "`Substrate with consumer.` New abstractions land with a runtime caller, test, benchmark, or other proof that the abstraction is consumed." (`docs/precepts/instructions/README.md:28-29`)

> "Every wave lands substrate with its consumer or deletes the substrate." (`docs/precepts/instructions/tranche/SPEC.md:53`)

> "`consumer will be wired later`" is an invalid hard gate. (`docs/precepts/instructions/tranche/SPEC.md:76`)

> "No stubs. No forward hooks without same-wave consumers." (`docs/precepts/instructions/tranche/WAVE_SPEC.md:153-154`)

Subject violation:

`docs/tranches/BB/waves/W0.md:78-82` creates `rank.rs` and `tiering.rs` as skeletons, says implementations land in W3, says the skeletons "produce stub results", and whitelists `tier_skeleton_w3_consumer`.

Why it matters:

This is exactly the substrate-forward pattern the hardening prompt names as chronic. It also trains `substrate_audit.rs` to accept a deferred consumer, weakening the permanent test before BB even starts.

Amendment direction:

Move rank/tiering surface creation to BB.W3, or make W0 consume the skeletons in a real admission-chain fixture in the same wave. The whitelist row `tier_skeleton_w3_consumer` should not exist.

### A2. BA permits HARD CAP extension after overrun

Edict:

> "Every triumvirate dispatch carries `HARD CAP: N min. At 0.9N commit, at N halt.`" (`docs/precepts/instructions/ORCHESTRATION.md:117-118`)

> "every dispatch prompt carries `HARD CAP: N min. At 0.9N commit, at N halt.`" (`docs/precepts/instructions/LESSONS-LEARNED.md:155-156`)

Subject violation:

`docs/tranches/BA/BA.md:128-129` first repeats the cap, then authorises the orchestrator to extend it after an in-flight write-authorized agent exceeds the cap without a commit. BB and BC top-level plans say "Same as BA" (`docs/tranches/BB/BB.md:106`, `docs/tranches/BC/BC.md:106`), so they risk inheriting the bad rule.

Why it matters:

The cap is a halt protocol, not an extension protocol. Extending an overrun preserves the exact diagnostic-loop and partial-work ambiguity the edict exists to stop.

Amendment direction:

Replace BA rule 11 with: "If a write-authorized agent reaches N without a clean owned commit, it halts and returns current state; the orchestrator either narrows bounds, dispatches triumvirate, or redeploys with an amended plan." BB/BC should restate the canonical cap instead of "Same as BA."

### A3. Codegen waves lack generated-size budgets

Edict:

> "every tranche that touches a code generator declares an expected output line-count window for each generated artefact." (`docs/precepts/instructions/LESSONS-LEARNED.md:285-286`)

> "wave docs that change generators include a `generated-size-budget` table in §Verification Artefacts." (`docs/precepts/instructions/LESSONS-LEARNED.md:290-291`)

Subject violation:

BA.W2 creates Rust emitter modules and regenerates all 9 grammar outputs (`docs/tranches/BA/waves/W2.md:13-16`, `docs/tranches/BA/waves/W2.md:46`, `docs/tranches/BA/waves/W2.md:93-97`), but its Verification Artefacts list no generated-size-budget table (`docs/tranches/BA/waves/W2.md:129-139`).

BB.W4 integrates rewrite discovery into regen and rewrites generated outputs (`docs/tranches/BB/waves/W4.md:76-90`), but its artefacts only include admission chain, LOC delta, regen, rule docs, and audit log (`docs/tranches/BB/waves/W4.md:107-113`).

Why it matters:

BA.W2 is precisely the kind of emitter wave that can create O(N) generated output blow-up. BB.W4's shrink requirement catches one positive row, but does not bound the other eight generated artefacts.

Amendment direction:

Add `generated-size-budget` tables to BA.W2 and BB.W4 Verification Artefacts. The table should list each generated grammar output, pre-wave LOC, expected post-wave window, overflow disposition, and close artefact path.

### A4. Status-tick cadence is missing from subject orchestration rules

Edict:

> "When sub-agents are running and the orchestrator has been silent to the user for ~5 minutes, emit a one-line status tick..." (`docs/precepts/instructions/ORCHESTRATION.md:181-184`)

Subject gap:

BA/BB/BC top-level rules encode hard caps, empty returns, worktrees, and triumvirate triggers (`docs/tranches/BA/BA.md:117-131`, `docs/tranches/BB/BB.md:104-106`, `docs/tranches/BC/BC.md:104-106`), but they do not name the 5-minute status-tick cadence.

Why it matters:

The hardening brief explicitly asks this lane to audit status-tick cadence. The plans leave it implicit in shared precepts, while they restate many other orchestration rules locally.

Amendment direction:

Add one top-level orchestration bullet to BA, BB, and BC: "Status ticks every ~5 minutes of orchestrator-silent wait while agents are running; each tick names live agents, worktrees, last transcript touch, and next decision point."

### A5. Per-wave doc reconciliation is under-specified before W6

Edict:

> "Every wave close includes documentation reconciliation... it is not optional." (`docs/precepts/instructions/tranche/DOC_UPDATE_WAVE.md:3-4`)

> "The next wave does not open until docs say the same thing as the worktree." (`docs/precepts/instructions/tranche/DOC_UPDATE_WAVE.md:27`)

Subject gap:

W6 specs explicitly update PROGRESS/FINAL and run close honesty (`docs/tranches/BA/waves/W6.md:18-21`, `docs/tranches/BB/waves/W6.md:14-16`, `docs/tranches/BC/waves/W6.md:14-16`). Earlier commit plans generally name audit docs and close status commits but not `PROGRESS.md`, wave status, and parent table reconciliation; sample BA.W2 commit plan (`docs/tranches/BA/waves/W2.md:141-145`) and BC.W0 commit plan (`docs/tranches/BC/waves/W0.md:111-115`).

Why it matters:

The docs can drift for six waves and then be "fixed" at W6, which is the failure the doc-update wave precept exists to prevent.

Amendment direction:

Add a standard line to every W0-W5 Commit Plan: "Wave-close doc/status commit updates `PROGRESS.md`, this wave's `Status`, the parent wave table, and the named audit artefacts before the next wave opens."

## Adherence Notes

- Agent cap: all active waves are 3, 5, or 6 agents; no wave exceeds six (`docs/tranches/BA/BA.md:92-98`, `docs/tranches/BB/BB.md:84-90`, `docs/tranches/BC/BC.md:84-90`).
- Worktree isolation: all 21 wave specs contain a Worktree Plan section, and sampled plans enumerate distinct `CARGO_TARGET_DIR` paths (`docs/tranches/BA/waves/W2.md:62-71`, `docs/tranches/BB/waves/W6.md:50-56`, `docs/tranches/BC/waves/W6.md:50-56`).
- Triumvirate presence: all 21 wave specs contain a Triumvirate Dispatch section; BA top-level repeats the mandatory auto-trigger conditions (`docs/tranches/BA/BA.md:130`).
- Deletion bias: BA/BB/BC each carry a deletion-bias section and name forbidden replacement patterns (`docs/tranches/BA/BA.md:160-178`, `docs/tranches/BB/BB.md:129-138`, `docs/tranches/BC/BC.md:126-136`).
- Heavy-surface routine defaults: the exact "NO heavy-surface routine defaults" edict from the older meta-audit exemplar is not present verbatim in the current edict files. The subject specs mostly use `--cargo-profile ax-iter` and focused nextest for routine verification; BA.W0's dev baseline names plain `cargo nextest run --workspace` as a measurement row (`docs/tranches/BA/waves/W0.md:17`), which should either become `--cargo-profile ax-iter` or be labelled close-proof/baseline, not routine iteration.

## Memory-Proposal Scope

None. The audit surfaced no new reusable feedback. All findings bind to existing precepts and existing project memories already reflected in `docs/precepts/instructions/LESSONS-LEARNED.md`.
