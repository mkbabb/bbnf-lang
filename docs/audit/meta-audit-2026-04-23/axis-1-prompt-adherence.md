# Axis 1 — Prompt Adherence

This report audits prompt adherence for the 2026-04-22..2026-04-23 audit arc itself. It is about whether the audit delivered what the user asked for, not whether every architectural claim was correct. Technical soundness belongs to the later axes.

The prompt surface was not one message; it was an escalation sequence. The earliest preserved prompts asked for a deep audit and a path forward without immediate execution. Later prompts widened that into commit archaeology, appurtenant-repo modernization, SOTA synthesis, tranche rewrites, and a GESTALT capstone. The key adherence question is therefore not "did one prompt get followed?" but "did the audit keep pace with the expanding request surface, and where did it stop at planning rather than execution?"

## Prompt Reconstruction

The reconstruction below is transcript-backed where possible. The locally preserved Claude transcript at `/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/4bec5721-12ea-4148-8a93-d6052152a90f/tasks/bf25lgzzz.output` contains the opening audit exhortations:

1. Deep audit + path forward, with no immediate execution:
   - `.../bf25lgzzz.output:29-37`:
     - "DEEPLY audit with 4 agents in parallel our original tranche plan and waves thereof, alongside our previous and next tranche plan"
     - "Devise a path forward"
     - "NO quick solutions, NO workarounds"
     - "NO legacy code"
     - "Do not immediately execute the plan thereupon."

2. B1/path-forward/toolchain-redress prompt:
   - `.../bf25lgzzz.output:148-148` asks for a progress document, a path-forward document, and an auditing/assay prompt to improve B0-era development time and fold that work into B1.

3. Session meta-audit prompt:
   - `.../bf25lgzzz.output:151-153` asks for a meta-audit of recent Claude sessions focused on prompt adherence, tranche scope/adherence, and compile/bench/test pain, with 4 agents in parallel updating `instructions/`, `tranches/`, extant tranche plans, and B1.

4. Architecture-pressure addenda:
   - `.../bf25lgzzz.output:269-277` asks whether CSP, e-graph, and structural-pattern machinery are actually wired and activating, and whether shared-cost analysis is unified.
   - `.../bf25lgzzz.output:391-401` asks for a deep recap of what was done, that deferred items be folded into the next tranche, and that direct-to-struct projection be made general and performant.

The later expansion prompt is not present in this Claude transcript file. Its clauses are reconstructable from the commissioning brief for this meta-audit, which explicitly lists the added asks the audit was supposed to answer:

5. Analyze the last ~900 / ~200 / ~2000 commits.
6. Retrospect the DTA/PSI interpreter rut.
7. Research and synthesize a SOTA union with recursive descent, sonic-rs, simdjson, lightningcss, and grammar-derived semantic parity.
8. Drive derivation through IR passes, type inference, CSP, and e-graph machinery.
9. Audit all appurtenant repos for toolchain and development modernization.
10. Fully abrogate bencher/criterion and ad hoc/hack workflow surfaces.
11. Produce a many-document synthesis culminating in a GESTALT-style universal overview.
12. Deploy 8 agents in several waves.

The rest of this report grades adherence against that reconstructed request surface.

## Verified

### 1. The audit did deliver the requested session meta-audit of friction, instruction adherence, tranche drift, and toolchain pain.

This was the most explicit transcript-backed ask, and it landed directly. `docs/tranches/meta-audit/01-session-friction.md:1-6` audits the recent session transcripts themselves; `02-instruction-adherence.md:1-12` compares observed orchestrator behavior against `docs/instructions`; `03-tranche-drift.md:1-8` audits plan/progress/doc drift across live tranches; `04-toolchain-pain.md:1-8` measures the compile/test/bootstrap bottlenecks. `05-validation.md:1-15` then re-checks the highest-value claims and integration edits. On this part of the ask, the audit was faithful and concrete.

### 2. The audit did produce the requested B1 progress/path-forward/toolchain-redress planning surface.

The transcript asked for a progress document, a path-forward document, and an auditing/assay prompt that would turn development-time redress into B1. That landed as a real tranche package, not an inline memo: `docs/tranches/B1/B1.md:1-28` defines B1 as the bounded prelude annex for dev-loop truth; `docs/tranches/B1/B1.md:104-121` defines the four-wave execution shape; `docs/tranches/B1/TOOLCHAIN-SOTA.md` and `docs/tranches/B1/TOOLCHAIN-MIGRATION.md:23-100` turn the toolchain question into a ranked decision ledger; `docs/tranches/B1/patches/*` supply concrete drafts. This directly satisfies the "write path forward / assay prompt / fold into B1" request.

### 3. The audit did deliver a real commit-history archaeology and a real DTA/PSI retrospective.

`docs/tranches/meta-audit/06-commit-archaeology.md:1-9` declares a retrospective taxonomy over the project history; `06-commit-archaeology.md:41-50` partitions the history into six eras; `06-commit-archaeology.md:207-220` contains the dedicated DTA/PSI-era anatomy section. The capstone narrative integrates the same conclusion: `docs/GESTALT.md:28-37` names Era V as the DTA/PSI rut and records its performance failure against AU. The request to analyze the rut as a real historical problem, not as a slogan, was met.

### 4. The audit did produce the requested SOTA synthesis and grammar-derived architectural union.

The user asked for a union of the best ideas from recursive descent, sonic-rs, simdjson, lightningcss, and the repo's own IR/CSP/e-graph work. That landed explicitly in `docs/GESTALT.md:755-840`, which names simdjson, sonic-rs, lightningcss, Ruler, egg, parse-that, and yyjson and explains what bbnf-lang is taking from each. The direct-to-struct tranche plans also operationalize the same thesis: `docs/tranches/AZ-I/AZ-I.md:21-49` defines grammar-derived direct-to-struct with parity harness recodes; `docs/tranches/AZ-II/AZ-II.md:17-42` makes BBNF self-hosting and tape deletion the completion point; `docs/tranches/BA/BA.md:13-53` and `docs/tranches/BB/BB.md:20-49` turn sonic-rs/simdjson ergonomics and e-graph/rule-inference into concrete successor tranches. This is not a token mention; it is a serious synthesis surface.

### 5. The audit did answer the "grammar derived and not hardcoded" demand with tranche-owned contracts.

This theme is now explicit across the audit output. `docs/GESTALT.md:105-156` states that grammar-derived semantics are the composition layer and that `project_types` writes the `StructRegistry`; `docs/tranches/AZ-I/AZ-I.md:21-26` says typed `->` annotations become emitted struct fields through IR type inference; `AZ-I/AZ-I.md:54-87` makes one codegen path, rich AST preservation, and no fallback/tape branch load-bearing invariants; `AZ-II/AZ-II.md:73-89` extends the same discipline to fleet-wide tape deletion; `BA/BA.md:15-21` makes path resolution a type-checked grammar-derived accessor; `BB/BB.md:33-42` states that rewrite rules are grammar-derived, not hand-authored in `crates/core`. The audit did not duck this request.

### 6. The audit did cover the appurtenant repo surface, not just the main workspace.

`docs/tranches/meta-audit/07-appurtenant-assay.md:1013-1039` counts the bench-framework posture across the appurtenant fleet; `07-appurtenant-assay.md:1128-1149` audits CI/nextest divergence across those repos; `07-appurtenant-assay.md:1270-1297` summarizes the modernization critical path. That work was then promoted into an execution-planning surface in `docs/tranches/next-tranche-research/repo-modernization/INDEX.md:1-17`, `INDEX.md:21-46`, and `INDEX.md:119-200`, plus 18 per-repo follow-on docs. The user asked for all appurtenant repos to be considered in toolchain/dev modernization, and the audit did that homework.

### 7. The audit did synthesize into a capstone "universal whole" overview.

The request for a gestalt overview was satisfied directly by `docs/GESTALT.md:1-55`, which states the current narrative, current sequence, and architecture in one place, and by `docs/GESTALT.md:1250-1272`, which indexes the worktree-agent deliverables that fed it. This is the clearest place where the audit exceeded "many docs" and actually produced the requested master narrative.

### 8. The audit met and exceeded the multi-agent dispatch requirement.

The early prompt required 4 agents in parallel; the later expansion requested 8 agents in several waves. The capstone records a larger operation than that. `docs/GESTALT.md:1252-1272` enumerates 11 worktree-branch deliverables across the prior validation wave and the two main audit waves. Even if one counts only the explicitly labeled W1 and W2 branches, the audit exceeded the later 8-agent bar.

### 9. The early "do not immediately execute the plan" instruction was obeyed.

This matters because the prompt sequence later tightened into implementation demands. On the initial audit-opening ask, though, the user explicitly said to audit and plan first and not immediately execute. The audit epoch from `48e6eaa9` to `5a260f94` is documentation-only: `git diff --stat 48e6eaa9..HEAD` reports 95 changed files, all under `docs/`, with no runtime code or toolchain edits. On the initial planning-only phase, adherence was exact.

## Refined

### 1. The "last 900 / 200 / 2000 commits" request was answered in substance, but not as three literal windowed reports.

The archaeology delivery chose a better organizational unit than the raw sliding windows: eras. `docs/tranches/meta-audit/06-commit-archaeology.md:3-5` and `06-commit-archaeology.md:41-50` turn the project into a six-era retrospective rather than three fixed windows. `docs/tranches/next-tranche-research/ARCHIVAL-SYNTHESIS.md:46-50` then abstracts "what worked" across that history, while `ARCHIVAL-SYNTHESIS.md:207-220` gives the DTA/PSI span. This is a real answer to the request, but it is not a literal "900 section / 200 section / 2000 section" document set.

### 2. The archaeology horizon widened over time, and that repaired an initial narrowing.

The first long-horizon synthesis narrowed the historical pool to the modern monorepo: `docs/tranches/next-tranche-research/ARCHIVAL-SYNTHESIS.md:15-22` says the "last N commits" pool is effectively 897 total with roughly 700 post-monorepo. The later archaeology corrected that by explicitly auditing the full history: `docs/tranches/meta-audit/06-commit-archaeology.md:3-5`, `06-commit-archaeology.md:28-33`. That sequence still counts as adherence, but only after a real refinement from "post-monorepo" to "full project history plus branch events."

### 3. The toolchain-framework question was answered by narrowing it to a stronger choice than the prompt named.

The user raised bencher vs criterion. The audit did not stay inside that box; it concluded that both are wrong primaries for this repo and selected divan plus nextest plus iai-callgrind. That narrowing is explicit in `docs/tranches/B1/TOOLCHAIN-MIGRATION.md:27-54` and `TOOLCHAIN-MIGRATION.md:115-119`. This is faithful to the spirit of the request because the user asked for SOTA toolchain modernization, not brand loyalty to criterion.

### 4. The friction-streamlining ask landed strongly at the instructions layer, but not as a net shrink of the total planning surface.

The streamlining pass itself is real. `docs/instructions/CHANGELOG.md:3-82` documents the contraction of `README.md` and `PROFILING.md` and records the "single authoritative location" policy. But the same audit epoch also added a large tranche and synthesis surface: `git diff --stat 48e6eaa9..HEAD` shows 18,188 insertions across 95 docs files. So the audit did streamline the orchestrator-first instruction layer, but it simultaneously expanded the tranche-planning corpus dramatically. That is a refinement of the claim "AI agent friction was reduced": it was reduced at the top-level instruction surface, not across the whole documentation estate.

## Flawed

### 1. The audit did not deliver the later "ACTUAL EDIT" version of B1.

This is the largest adherence miss. The later expansion prompt did not ask only for analysis; it explicitly asked to refine B1 with the actual edit so that building, checking, expanding, benching, and testing become rapid again. The audit authored that plan in depth, but did not land it. Evidence:

- B1 promises pinning and bench-harness replacement: `docs/tranches/B1/B1.md:32-55`, `B1/B1.md:104-121`, `docs/tranches/B1/TOOLCHAIN-MIGRATION.md:154-240`.
- HEAD still has no `rust-toolchain.toml` file.
- HEAD still has `bencher = "0.1"` in `crates/core/Cargo.toml:42`.
- HEAD CI still runs `cargo test --workspace` in `.github/workflows/ci.yml:41-46`.
- HEAD `.cargo/config.toml:58-115` still carries the pre-B1 alias surface rather than the rewritten B1 one.

The audit therefore delivered a strong implementation blueprint, but not the implementation the later prompt demanded.

### 2. "Bencher and criterion fully abrogated" did not happen; only the abrogation plan happened.

The audit is explicit about the desired future state. `docs/tranches/B1/TOOLCHAIN-MIGRATION.md:104-140` says `bencher` is removed, criterion is rejected, and nextest becomes required. `docs/tranches/meta-audit/08-abrogation-catalog.md:1-10` frames the whole exercise as explicit KEEP/REPLACE/ABROGATE decisions. But current repo truth still contradicts literal abrogation:

- `docs/tranches/meta-audit/07-appurtenant-assay.md:1013-1039` counts 8 criterion benches, 40 bencher benches, 4 `test::Bencher` benches, and zero divan benches at audit time.
- `crates/core/Cargo.toml:42` still declares `bencher = "0.1"`.
- `crates/json-prototype/Cargo.toml:23` still declares `bencher = "0.1"`.
- `../parse-that/rust/parse_that/Cargo.toml:102` still declares `bencher = "0.1.5"`.
- `../csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/Cargo.toml:14` and `.../morph-core/Cargo.toml:13` still declare criterion.

This is not a minor lag. The user's wording was literal. The audit planned the abrogation well and did not execute it.

### 3. "Ad hoc and hacks non modern arch fully abrogated" also did not happen; the catalog stops one stage short.

`docs/tranches/meta-audit/08-abrogation-catalog.md:1-32` is a serious classification exercise over 63 items. But classification is not abrogation. The same document repeatedly routes actual deletion or replacement to later work, for example `08-abrogation-catalog.md:42-61` (`bench_regression.sh`), `08-abrogation-catalog.md:80-100` (`bootstrap-bbnf.sh` modernization), and the B1 plan's own invariant `docs/tranches/B1/B1.md:89-91`, which says deletion waits until B1 replacement commits land. Against the literal exhortation, the audit stopped at catalog + tranche plan.

### 4. The audit did not actually "get out of the local rut" on dev-loop speed; it only named the exit.

The user's problem statement was not abstract. It complained that compile, test, and bench times were "totally unacceptable" and needed to be rethought from first principles. The audit measured and explained the rut well: `docs/tranches/meta-audit/04-toolchain-pain.md:12-28` and `04-toolchain-pain.md:32-80` capture the cold-wall behavior and the bootstrap/lock-contention pain. But the repo still runs on that same rut-bearing surface today, because B1 has not executed. The audit succeeded as diagnosis and failed as remediation.

## Open

### 1. The later expansion prompt is only partially verbatim-reconstructed from local artifacts.

The opening audit prompts are transcript-backed. The widened clauses about the 900/200/2000 split, bencher/criterion full abrogation, the GESTALT universal whole, and 8-agent multi-wave execution are reconstructable from the commissioning brief, but not from the preserved Claude transcript file I could access locally. That does not invalidate the adherence findings above, but it does mean the later clause wording is reconstructed from the meta-audit brief rather than quoted from a raw session artifact.

### 2. The AU-baseline recovery request is answered at the planning level, not yet at the proof level.

The audit absolutely produced a path back: `docs/GESTALT.md:38-55` lays out `B1 -> AY-II -> AZ-I -> AZ-II -> BA -> BB`, and the direct-to-struct/tape-deletion tranches are written in detail. What remains open is execution proof. Axis 1 can credit the audit for producing the plan, but cannot claim that the requested recovery has been demonstrated yet.

### 3. The audit's own planning success increases the burden on later axes.

Because the audit did produce a very large, highly structured plan surface, prompt adherence now depends on whether the next step is honest about mode. If the project treats this audit as a planning tranche, adherence is strong. If it treats this audit as already satisfying the implementation asks, adherence is weak. That mode distinction is now load-bearing and should be made explicit in the capstone index.

## Axis 1 Verdict

The audit delivered the analysis-and-planning asks: session archaeology, tranche drift audit, toolchain diagnosis, appurtenant-repo assay, historical retrospective, DTA/PSI reckoning, SOTA synthesis, tranche rewrites, instructions streamlining, and a real GESTALT capstone. It also met the multi-agent orchestration ask and respected the earliest "plan first, do not execute immediately" instruction.

The audit did not deliver the later escalation from planning to implementation. B1 is now well-specified, but still unlanded. Bencher/criterion abrogation is now well-specified, but still unlanded. Ad hoc-script abrogation is now cataloged, but still unlanded. The honest adherence judgment is therefore: **strong on analysis, strong on synthesis, strong on planning, not yet sufficient on execution where the prompt surface later demanded actual edits.**
