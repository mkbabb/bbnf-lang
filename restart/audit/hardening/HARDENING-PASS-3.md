# HARDENING-PASS-3 - Runtime/user-surface double-back audit

## §1 Target identification

| Field | Value |
|---|---|
| Target | PASS-3 |
| Primary target | `restart/audit/pass-3-runtime/PASS-3.md` |
| Sub-agent targets | `restart/audit/pass-3-runtime/agent-1-value-api-designer.md`; `agent-2-path-select-dsl-designer.md`; `agent-3-visitor-surface-designer.md`; `agent-4-tape-union-architect.md`; `agent-5-error-recovery-incremental-parsing.md`; `agent-6-ecosystem-architect.md` |
| Commit audited | `015317db` |
| Lines audited | 1,011 target-output lines: 371 synthesis lines plus 640 sub-agent lines |
| Required context read | `restart/README.md`, `restart/locks/LOCKS.md`, precepts, PASS prompts, corpora, inheritance index, PASS-3 outputs |
| Grep checks | `rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math' restart/audit/pass-3-runtime/PASS-3.md restart/audit/pass-3-runtime/agent-*.md`; `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/pass-3-runtime/PASS-3.md restart/audit/pass-3-runtime/agent-*.md` |
| Time consumed | ~38 minutes |

The target gets the central substrate correction right: tape is named tape, is unioned with direct-to-struct, and `ParseStream` is rejected as a stale prompt/inheritance word at `restart/audit/pass-3-runtime/PASS-3.md:16-23`, `restart/audit/pass-3-runtime/PASS-3.md:31-32`, and `restart/audit/pass-3-runtime/PASS-3.md:309-310`. The audit therefore does not relitigate tape. It challenges the target's adherence at the user surface, crate names, SOTA gates, future-grammar proof, generated budget, and carry discipline.

## §2 Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 - Lock-Adherence | AMENDMENT-REQUIRED | 7 | 7 | 0 | Keep tape/direct union; amend path naming, `pointer!`, SOTA numbers, Lock 13 tree, and Lock 14 yaml/per-X proof. |
| 2 - Sequencing Discipline | N/A | 0 | 0 | 0 | Single PASS. No multi-wave sequence claims. |
| 3 - Cohesion | AMENDMENT-REQUIRED | 2 | 6 | 0 | Convert unresolved hand-offs into concrete gates and stop punting final crate names and bench targets. |
| 4 - SOTA Anchoring | AMENDMENT-REQUIRED | 1 | 5 | 0 | Replace citation-only benchmark rows with exact competitor/dataset/platform/target numbers. |
| 5 - Grammar-Authoritative Discipline | AMENDMENT-REQUIRED | 3 | 5 | 0 | Add yaml onboarding, per-X tables, and unprefixed path/test-fixtures crates; keep zero match arms. |
| 6 - Generated-Code + LOC Budget | AMENDMENT-REQUIRED | 0 | 5 | 0 | Add generated LOC ceilings, per-grammar deltas, and xtask regen-cycle wall budgets. |
| 7 - Friction Forecast | AMENDMENT-REQUIRED | 2 | 7 | 0 | Add verbatim messages for `pointer!`, lifetime, layout-lowering, Pratt/SIMD, crate split, incremental, and yaml. |
| 8 - Carry & Deferral Audit | AMENDMENT-REQUIRED | 0 | 7 | 0 | Every carry needs receiver, blocker, and receiving gate. |
| 9 - Greenfield Discipline | AMENDMENT-REQUIRED | 4 | 5 | 0 | Remove vestigial prefixing, registry framing, marketing-grade benches, and unbounded aggregator growth. |
| **Total** | **AMENDMENT-REQUIRED** | **19** | **47** | **0** | **Twelve surgical amendments before SYNTHESIS consumes PASS-3.** |

Final decision: **AMENDMENT-REQUIRED**.

The target is not a re-draft. Its core posture survives: tape, direct-to-struct, slice-borrow constructors, visitors, recovery, incremental snapshots, and no default per-grammar declaration crates. The amendments are concentrated and mechanical enough to dispatch as a narrow PASS-3 amendment: rename the authored path surface to `pointer!`, repair crate names, add exact gates, and close the proof obligations.

## §3 Lane 1 - Lock-Adherence

Lane standard: each lock is settled. This lane asks whether PASS-3 honours the lock in the surfaces it touches and names the exact surgery when it does not.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-3-runtime/PASS-3.md:16-23`; `agent-4-tape-union-architect.md:5`; `restart/locks/LOCKS.md:34` | Lock 1 - tape + direct-to-struct | PASS-3 rejects `ParseStream`, keeps tape, and marks stale prompts/inheritance. | Obeys current authority and resolves the prompt conflict directly. | Tape ABI and same-consumer proof remain delegated. | A stricter auditor could demand ABI in PASS-3, but PASS-3 owns user semantics and states the PASS-1/PASS-2 hand-off at `PASS-3.md:307-319`. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:139`; `agent-5-error-recovery-incremental-parsing.md:61`; `restart/locks/LOCKS.md:36` | Lock 2 - layout lowering | PASS-3 mentions `@layout` and formatter metadata, but it does not give a layout-lowering error for unresolved rule layout. | It avoids retired names such as `TypeMap` and `StructLayout` in the target. | The friction lane expressly requires layout-lowering errors; PASS-3 gives only an unused-layout formatter warning. | Add the rule-level diagnostic now so SYNTHESIS does not inherit a silent layout-fault surface. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:80`; `restart/corpora/RESTART-SKETCH.md:614-624`; `restart/locks/LOCKS.md:38` | Lock 3 - cursor + byte-skip unified | PASS-3 says path/select traverse the tape cursor, but it never states eager empty-path branch elision or byte-skip inside cursor. | It does not propose a second parser. | It also does not preserve the one-parse implementation invariant in its runtime API gates. | Add a PASS-2 hand-off gate: eager parse must elide cursor calls, lazy path skips through the same implementation. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:141-158`; `restart/locks/LOCKS.md:40` | Lock 4 - orthogonal optimisation | PASS-3 does not fuse CSP/egraph/cost-model or invent a hypergraph in incremental parsing. | Properly leaves optimisation internals to PASS-1 and user-visible snapshots to PASS-3. | Incremental cache reuse mentions semantic/egraph survival only indirectly via prompts, not the target. | No PASS-3 surgery needed; SYNTHESIS still must keep PASS-1 as authority. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:316-319`; `restart/README.md:108-113`; `restart/locks/LOCKS.md:42` | Lock 5 - Backend IR contract | PASS-3 requests emitted runtime surfaces from PASS-2 rather than walking grammar IR itself. | Keeps codegen as the owner of generated visitors, metadata, and path schema. | It does not restate Backend IR as the formal boundary. | Add one sentence under §8 naming Backend IR as the only PASS-2 emission contract. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:84-92`; `restart/locks/LOCKS.md:44` | Lock 6 - xtask source artefacts | PASS-3 uses proc-macros only for path/select, an allowed shell, and does not propose proc-macro parser generation. | No violation of committed-source discipline. | It omits regen wall budget, covered in Lane 6. | Keep the proc-macro exception fenced to path/select shells. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:88-90`; `PASS-3.md:244-270`; `restart/README.md:50-52`; `restart/locks/LOCKS.md:46` | Lock 7 - path crate consolidation | PASS-3 names `bbnf-path-core`, `bbnf-path`, and `bbnf-path-ts` instead of `path`, `path-core`, and `path-ts`. | It correctly extracts shared core and eliminates duplicated TS compiler logic. | It violates the settled unprefixed internal crate shape and punts naming to SYNTHESIS at `PASS-3.md:324`. | Rewrite the target to unprefixed crates; do not carry a naming fork. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:291-303`; `restart/README.md:328-333`; `restart/corpora/SOTA.md:50-56`; `restart/corpora/SOTA.md:130-136`; `restart/locks/LOCKS.md:48` | Lock 8 - SOTA gates | PASS-3 says every gate names competitors, but its table names datasets and citations without numbers or platforms. | It chooses the right competitor families. | The actual gate is unverifiable: no `≤ 380 µs`, `≤ 750 µs`, `≤ 2.8 ms`, `≤ 3.0 ms`, `≤ 1.6 ms`, or GB/s row appears. | Replace the table with the README §9 target table and corpus citations. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:49-71`; `agent-1-value-api-designer.md:13-15`; `restart/locks/LOCKS.md:50` | Lock 9 - slice borrow + arena + owned | PASS-3 preserves `parse`, `parse_in`, and `parse_owned`. | The three surfaces are clear and routed to benchmarks. | Arena trait details remain PASS-2-owned. | The hand-off is sufficient for PASS-3. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:303`; `agent-5-error-recovery-incremental-parsing.md:64-66`; `restart/README.md:180-182`; `restart/locks/LOCKS.md:52` | Lock 10 - Pratt/SIMD auto-detected | PASS-3 does not add `@pratt` or `@simd`, but it does not include the required misfire diagnostics. | Settled auto-detection is not violated. | User-facing failure mode is silent. | Add verbatim `BBNF-OPT00x` diagnostics for Pratt and SIMD auto-detection outcomes. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:321-325`; `restart/README.md:53-58`; `restart/locks/LOCKS.md:54` | Lock 11 - path-deps for sister crates | PASS-3 does not alter sister-crate incubation. | No path-dep conflict introduced. | The `bbnf-path*` names still need Lock 7 surgery. | No additional Lock 11 surgery. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:160-289`; `restart/README.md:402`; `restart/locks/LOCKS.md:56` | Lock 12 - archive ceremony | PASS-3 does not interleave `ser`/`gorgeous` archive with runtime design. | It stays within top-layer scope. | It does not mention archive precondition, but this PASS does not own it. | No PASS-3 surgery. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:162-179`; `agent-6-ecosystem-architect.md:24-42`; `restart/README.md:96-100`; `restart/locks/LOCKS.md:58` | Lock 13 - no god directories | `bbnf/src/` lists 14 immediate children, exceeding the 4-10 child rule. | The named concerns are real user surfaces. | The aggregator becomes the very dumping-ground Agent 6 warned against at `agent-6-ecosystem-architect.md:13`. | Collapse into cohesive child directories, e.g. `parse/`, `document/`, `query/`, `diagnostics/`, `metadata/`, `prelude.rs`, `lib.rs`. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:289`; `PASS-3.md:360-367`; `restart/README.md:13-25`; `restart/README.md:396`; `restart/locks/LOCKS.md:60` | Lock 14 - full grammar generalisation | PASS-3 rejects default declaration crates and grammar match arms, but lacks yaml onboarding and per-X tables for every "generated grammar" claim. | The zero-match-arm grep passed, and per-grammar crates are discarded. | Future grammar proof is absent; fixture examples name only four grammars. | Add the `yaml.bbnf` two-surface test and per-X matrix. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 7 / REINVENT 7 / DISCARD 0.

## §4 Lane 2 - Sequencing Discipline

Lane standard: this lane applies to multi-wave targets. PASS-3 is a single PASS synthesis and does not draft tranche waves. The only sequencing auditable here is whether it makes multi-wave claims; it does not.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-3-runtime/PASS-3.md:305-327` | Cross-pass hand-offs | PASS-3 names PASS-1/PASS-2/SYNTHESIS hand-offs, not a wave sequence. | Lane 2 can be marked N/A without masking a hidden wave plan. | Some hand-offs become Lane 8 carries. | Treat hand-offs under carry discipline, not sequencing. | KEEP |

Lane verdict: **N/A for a single PASS**. KEEP 0 / REINVENT 0 / DISCARD 0.

## §5 Lane 3 - Cohesion

Lane standard: every claim in PASS-3 must be verifiable from the target or cited artefacts. Orphan claims and orphan deliverables become amendments.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-3-runtime/PASS-3.md:16-23`; `agent-4-tape-union-architect.md:74` | Stale authority reconciliation | PASS-3 identifies stale prompt/README/inheritance conflicts and chooses tape. | The claim is supported by direct citations and repeated sub-agent agreement. | It leaves the actual prompt/README/inheritance text for SYNTHESIS. | The target cannot edit inputs; marking them stale is sufficient. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:29-39`; `PASS-3.md:328-358` | Verdict ledger | The ledger mixes combined verdicts such as `KEEP / REINVENT`. | It captures nuance across sub-agent findings. | It prevents clean totals and makes punch-list ownership harder. | Split mixed rows into one primary verdict and a named surgery row. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:88-90`; `PASS-3.md:324`; `restart/README.md:50-52` | Final path crate names | PASS-3 proposes prefixed names, then defers naming to SYNTHESIS. | The implementation split is cohesive. | The naming fork is already settled by README and Lock 7. | Rewrite now; SYNTHESIS should consume, not decide, this settled name. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:291-303`; `PASS-3.md:366` | Bench targets | PASS-3 says benches must include competitor data and later says exact numbers land after PASS-1/PASS-2. | It recognises mode labels and trace overhead. | It has no verifiable close gate. | Add exact target numbers now; PASS-1/PASS-2 can refine implementation, not the external bar. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:160-289`; `restart/README.md:100` | Module tree output | PASS-3 provides crate trees for the required PASS-3 crates. | The deliverable exists and is easy to synthesize. | The `bbnf` tree violates child-count, and path/test-fixtures names are stale. | Keep the tree but restructure before it becomes ARCHITECTURE input. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:139`; `agent-5-error-recovery-incremental-parsing.md:13-16` | `@recover` surface | PASS-3 includes `@recover`, which is not in the current settled extension list. | It inherits existing recovery directive work and user-facing recovery semantics. | The current settled authority names `@error` and `@layout`, not `@recover`; `@recover` risks becoming a second directive surface. | Fold `@recover` into `@error(skip | recover | halt)` unless SYNTHESIS explicitly ratifies a compatibility alias. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:289`; `agent-6-ecosystem-architect.md:162` | Data-only fixtures | Fixture Rust code is grammar-agnostic, and per-grammar directories are declared data/manifests only. | This is consistent with `restart/README.md:58` and Amendment 01. | The synthesis lists only four fixture dirs and no yaml onboarding. | Keep the data-only rule; add yaml test row. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:360-367` | Unresolved punch-list | PASS-3 names six unresolved items. | The list is honest. | It lacks owner gates and exact edits. | Replace with a gate-ready punch list before SYNTHESIS consumes it. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 2 / REINVENT 6 / DISCARD 0.

## §6 Lane 4 - SOTA Anchoring

Lane standard: parse-throughput gates must cite competitor, dataset, platform, and number. Non-throughput gates must not claim Lock 8 honour.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/corpora/SOTA.md:50-56`; `restart/README.md:328-330` | JSON target numbers | Corpus gives sonic-rs/simd-json M1 Pro numbers; README sets bbnf targets. | Ground truth is available. | PASS-3 table cites only `SOTA.md:54-56` and omits the numbers. | Put `twitter <= 380 us vs sonic-rs 436 us / simd-json 424 us on M1 Pro`, `citm <= 750 us vs 854/831 us`, `canada <= 2.8 ms vs 3.144/3.226 ms`. | REINVENT |
| `restart/corpora/SOTA.md:130-136`; `restart/README.md:331-332` | CSS target numbers | Corpus gives lightningcss bootstrap/animate numbers. | Correct peer and datasets. | PASS-3 merges bootstrap/animate into one citation row without target numbers. | Split rows: `bootstrap <= 3.0 ms vs lightningcss 4.16 ms`; `animate <= 1.6 ms vs lightningcss 1.97 ms` on the ratified platform. | REINVENT |
| `restart/corpora/SOTA.md:83-88`; `restart/README.md:333` | simdjson On-Demand sustained gate | README carries 7 GB/s Intel and bbnf M-series/x86 targets. | Important tape/on-demand pressure. | PASS-3 has no sustained throughput row. | Add sustained parse row: `>= 5 GB/s M-series`, `>= 7 GB/s x86`, competitor `simdjson On-Demand 7 GB/s Intel Skylake`. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:293`; `restart/locks/LOCKS.md:48` | Lock citation | PASS-3 cites `restart/locks/LOCKS.md:207` for competitor baselines. | The intent is right. | Line 207 is a failure-mode clause in the lock file, not Lock 8. | Replace with `restart/locks/LOCKS.md:48` and README/corpus line citations. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:303`; `agent-6-ecosystem-architect.md:176` | Bench mode labels | PASS-3 requires borrowed/arena/owned, trace, projection, visitor, incremental fallback, and DAP overhead labels. | Good measurement hygiene. | Labels without numeric gates do not satisfy SOTA anchoring. | Keep labels as report columns after exact gates are added. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:301`; `restart/README.md:334` | BBNF self-host row | PASS-3 treats BBNF corpus as no-overfit rather than throughput. | Correctly avoids fake SOTA peer. | It omits README's `< 100 ms full self-parse + format roundtrip` internal gate. | Add it as a non-SOTA internal gate and explicitly say Lock 8 does not attach. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 1 / REINVENT 5 / DISCARD 0.

## §7 Lane 5 - Grammar-Authoritative Discipline

Lane standard: generic crates carry no grammar-specific code; all "every grammar" claims must be proven by per-X tables and the `yaml.bbnf` two-surface test. Grep results: grammar-name scan produced examples, fixture/data dirs, benchmark datasets, and audit citations; the match-arm regex returned zero matches.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| grep result | Match-arm regex | No `match grammar { Json => ... }` style arm appears in PASS-3 target outputs. | This satisfies the hard zero for proposed generic crates. | It does not prove future grammar onboarding. | Keep the zero-match invariant as a gate. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:45-53`; `agent-2-path-select-dsl-designer.md:30-35` | Grammar-named examples | `Json::parse` and `CssStylesheet` examples are illustrative. | Examples help users. | Without a per-X table they can be mistaken for privileged plan logic. | Mark examples as illustrative and add metadata-driven generic examples using `<Grammar>`. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:272-289`; `agent-6-ecosystem-architect.md:145-162` | Fixture dirs | Fixture directories are named `json`, `css`, `bbnf`, `sheets` but declared data/manifests only. | Data-only dirs are allowed by `restart/README.md:58`. | The list omits all nine extant grammars and the yaml onboarding proof. | Replace the four-dir sketch with a per-X fixture table and add yaml. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:289`; `agent-1-value-api-designer.md:18`; `agent-6-ecosystem-architect.md:20` | No default declaration crates | PASS-3 discards per-grammar declaration crates by default. | Honours current authority and Amendment 01. | Rare host adapter escape policy is left unresolved at `PASS-3.md:367`. | Keep default ban; fence escape in metadata with review gate. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:360-367`; `restart/README.md:13` | Future grammar onboarding | PASS-3 has no `yaml.bbnf` two-surface test. | It recognises full grammar generalisation in prose. | Lock 14 proof is absent. | Add a verbatim yaml onboarding gate: source file + metadata block only; zero Rust edits; path/select/visitor/fixtures work. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:42-80`; `PASS-3.md:333-338`; `restart/README.md:452` | Per-X tables | PASS-3 says "generated grammars" and "every public node" but gives no per-X matrix. | The abstraction is correct. | Per-X absence violates the README voice/discipline rule for all-X claims. | Add a table for all nine extant grammars plus yaml, with value API, visitor, path schema, fixture, host-fn route. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:88-90`; `restart/README.md:50-52` | Path crate prefix | Generic path crates are named `bbnf-path*`. | The split is useful. | Prefixing generic path crates violates the workspace shape. | Rename to `path`, `path-core`, `path-ts` throughout PASS-3. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:139`; `agent-5-error-recovery-incremental-parsing.md:14` | `@recover` | Recovery is inherited from extant analysis code. | Existing user concept can survive as behaviour. | As a grammar directive, it is not in the settled extension list. | Route it under `@error(recover)` or explicitly label legacy alias. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 3 / REINVENT 5 / DISCARD 0.

## §8 Lane 6 - Generated-Code + LOC Budget

Lane standard: every generated surface needs LOC budget, per-grammar delta, and regen-cycle wall budget. The historical generated baseline is visible in `restart/corpora/MODULES.md:619-629`, including `css_l4.rs` at 107,138 LOC and the nine generated parsers.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-3-runtime/PASS-3.md:316`; `agent-3-visitor-surface-designer.md:24-44` | Generated visitors | PASS-2 must emit visitors and bitflags. | Correctly generated, not hand-written. | No LOC ceiling for deep grammars. | Add per-grammar visitor LOC projection and `VisitTypes` bitset table. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:84-92`; `PASS-3.md:316`; `restart/corpora/MODULES.md:629` | Path metadata/schema | PASS-3 moves path registries to generated metadata. | Removes overfit registries. | No budget for metadata Rust, JSON sidecars, or TS schema output. | Add `path-schema` generated LOC and sidecar byte budget by grammar. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:107-133`; `agent-4-tape-union-architect.md:21-59` | Tape/direct projection | Direct nodes carry tape identity. | Necessary for one runtime identity. | No generated field/method delta estimate. | Add per-grammar delta for `TapeId`, span, `ValueRef`, direct projection, and round-trip tests. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:303`; `agent-6-ecosystem-architect.md:164-176` | Bench/report generated data | Bench harness includes datasets and competitors. | Useful close evidence. | No wall-clock budget or fixture-count budget. | Add `cargo xtask regen --check` wall budget and `bbnf-bench` report generation budget. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:360-367`; `restart/corpora/MODULES.md:591-594` | PASS-3 punch-list budget | PASS-3's unresolved list has no generated-size closure. | It admits remaining work. | Generated-code growth is invisible. | Add one §6 row: current generated baseline, target delta, max allowed regression, command to verify. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 0 / REINVENT 5 / DISCARD 0.

## §9 Lane 7 - Friction Forecast

Lane standard: every user-facing confusion point needs the API surface, mental model, confusion point, artefact, and verbatim diagnostic.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-3-runtime/PASS-3.md:84-92`; `restart/README.md:280-283` | `path!` vs `pointer!` | PASS-3 uses `path!`; settled user API names `pointer!` and `select!`. | `path!` reflects extant crate history. | It breaks the sonic-rs idiom explicitly chosen in README. | Rename authored API to `pointer!`; keep internal `PathPlan` type names. | REINVENT |
| `agent-2-path-select-dsl-designer.md:43-52` | Path diagnostics | Agent 2 gives three verbatim path diagnostics. | Concrete and useful. | They name path, not pointer; no grammar-inference message for implicit grammar. | Keep messages after renaming codes/text to `BBNF-POINTER00x`. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:49-71`; `agent-1-value-api-designer.md:69` | Lifetime constructors | PASS-3 gives signatures and cites cookbook. | Good API split. | No verbatim PASS-3 diagnostic for wrong constructor choice. | Add `BBNF-LIFE001` and `BBNF-LIFE002` messages for borrowed value escaping and arena mismatch. | REINVENT |
| `agent-5-error-recovery-incremental-parsing.md:61-62`; `restart/locks/LOCKS.md:36` | Layout-lowering error | Existing message is about unused formatter metadata. | It catches a real doc/formatter case. | It does not cover "rule X has no resolvable layout because Y." | Add `error[BBNF-LAYOUT002]: rule \`X\` has no resolvable layout; reason: ...`. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:303`; `restart/README.md:180-182` | Pratt/SIMD misfire | PASS-3 requests fallback rate and traversal timings, not optimizer diagnostics. | It does not require grammar authors to annotate. | Misclassification will be inscrutable. | Add `BBNF-OPT001` Pratt-not-applied and `BBNF-OPT002` SIMD-not-applied diagnostics with cost-model reason. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:324`; `agent-2-path-select-dsl-designer.md:62` | Crate split migration | PASS-3 leaves package names unsettled. | It recognises old docs use `bbnf-path`. | Users will import the wrong crate names. | Add migration page entry: `bbnf_path::path!` -> `path::pointer!`; TS binding -> `path-ts`. | REINVENT |
| `agent-3-visitor-surface-designer.md:57-68` | Visitor diagnostics | Agent 3 gives verbatim visitor warnings/errors. | Strong friction coverage for mutation and skipped recovery. | Synthesis does not carry them into §6. | Lift them into PASS-3 synthesis. | KEEP |
| `agent-5-error-recovery-incremental-parsing.md:64-68`; `PASS-3.md:158` | Incremental fallback | PASS-3 says dev output must report fallback rates. | Good operational truth. | The diagnostic is only a warning and lacks receiver docs. | Add dev-only trace wording and an LSP policy: not user-spam. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:360-367`; `restart/README.md:13` | Adding yaml | No user-facing new-grammar error or cookbook row exists. | PASS-3 knows fixtures are data-only. | Grammar authors get no route when metadata is missing. | Add `BBNF-GRAMMAR001`: metadata block missing for `yaml.bbnf`; help text names the two surfaces. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 2 / REINVENT 7 / DISCARD 0.

## §10 Lane 8 - Carry & Deferral Audit

Lane standard: every carry must name receiver, blocker, and receiving gate. A carry without all three is a fault.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-3-runtime/PASS-3.md:362` | Stale prompt/README/inheritance reconciliation | Receiver is SYNTHESIS. | Correct receiver. | No blocker or receiving gate. | Add gate: `SYNTHESIS ARCHITECTURE §11 input-normalisation table has zero ParseStream/rewrite-mode/grammar-Unicode API surfaces`. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:363`; `agent-4-tape-union-architect.md:91-95` | Tape ABI confirmation | Receiver is PASS-1. | Correctly outside PASS-3 layout authority. | No receiving gate. | Add gate: `PASS-1 §4 Tape ABI table covers token byte layout, payload arena, span width, sibling skip, snapshot id`. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:364`; `agent-2-path-select-dsl-designer.md:60` | Generated metadata schema | Receiver is PASS-2. | Correct codegen owner. | No blocker stated beyond "must confirm." | Add blocker: PASS-3 cannot type-check `pointer!`/visitors until Backend IR metadata emit exists; receiving gate in PASS-2 §4. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:365`; `agent-2-path-select-dsl-designer.md:62` | Final workspace naming | Receiver is unnamed final workspace/SYNTHESIS. | It exposes a conflict. | Settled README already decides unprefixed names; carrying it invites drift. | Do not defer; amend PASS-3 directly. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:366`; `restart/README.md:328-336` | Bench exact numbers | Receiver is bench harness after PASS-1/PASS-2. | Platform profiles may need measurement. | Competitor target numbers already exist; no reason to defer them. | Amend PASS-3 now with fixed gates; leave only local baseline measurement to bench owner. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:367`; `agent-1-value-api-designer.md:73` | Rare host adapter escape | Receiver is absent. | The risk is real. | Without a receiver/gate, exception policy can become declaration crates again. | Add receiver: SYNTHESIS ARCHITECTURE metadata schema; blocker: host primitive cannot express adapter; gate: no `crates/<grammar>/` default, exception table empty for extant nine. | REINVENT |
| `agent-5-error-recovery-incremental-parsing.md:86`; `PASS-3.md:141-158` | Incremental parser alignment | Receiver is SYNTHESIS plus PASS-1/PASS-2. | User-visible behaviour is named. | No receiving gate for fallback-rate reporting or snapshot ID compatibility. | Add gate in PASS-3 punch list: incremental bench reports fallback rate and snapshot reuse by dataset. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 0 / REINVENT 7 / DISCARD 0.

## §11 Lane 9 - Greenfield Discipline

Lane standard: no quick solutions, no workarounds, no legacy code uncontested, no contrivance, no overfit, and no needless complication.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-3-runtime/PASS-3.md:16-23`; `agent-4-tape-union-architect.md:101-105` | Tape authority | PASS-3 cuts the stale anti-tape and `ParseStream` residue. | Greenfield posture is clear and rooted in current authority. | Tape can become bloated if trace is not fenced. | Optional trace and compact token layout defeat the challenge. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:88-90`; `PASS-3.md:244-270` | Prefix carry-over | `bbnf-path*` names survive from extant crates. | Easier inheritance from existing code. | It carries a legacy name against the fresh workspace shape. | Greenfield starts with `path*`; compatibility shims are not needed pre-execution. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:84`; `restart/corpora/CENSUS.md:143-149` | Registry reinvention | PASS-3 identifies hardcoded registries and replaces them with metadata. | This is exactly the root-cause fix. | It still phrases the current code as a demonstrator rather than a kill-list item in synthesis. | Keep metadata route; make deletion a gate. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:160-179`; `agent-6-ecosystem-architect.md:13` | Aggregator sprawl | `bbnf` owns 14 children. | It gathers related public API. | This recreates a god-directory risk at the user-facing crate. | Partition by cohesive modules before ARCHITECTURE consumes it. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:293-303` | Benchmark posture | PASS-3 requires competitor labels and raw mode labels. | It resists marketing benches. | Missing numbers leave the gate aspirational. | Add the exact external floors. | REINVENT |
| `agent-5-error-recovery-incremental-parsing.md:13-18`; `PASS-3.md:139` | Recovery directive | PASS-3 keeps recovery and rebuilds on generated metadata. | Preserves useful authoring intent without full-reparse permanence. | `@recover` as separate surface risks extension sprawl. | Fold into `@error(recover)` or mark compatibility alias. | REINVENT |
| `restart/audit/pass-3-runtime/PASS-3.md:316-319`; `agent-4-tape-union-architect.md:95` | Host function route | PASS-3 keeps host functions in metadata/primitives, not declaration crates. | Stops per-grammar crate spread. | Escape policy unresolved. | Fence rare adapters with metadata and review gate. | KEEP |
| `restart/audit/pass-3-runtime/PASS-3.md:141-158`; `agent-5-error-recovery-incremental-parsing.md:90-94` | Incremental fallback honesty | PASS-3 permits full-parse fallback but requires reporting fallback rates. | Honest engineering: correctness first, measured degradation. | Without gates it becomes a workaround. | Add dataset-level fallback thresholds and trace evidence. | REINVENT |
| `agent-3-visitor-surface-designer.md:46`; `PASS-3.md:94` | Mutation via visitors | Mutation stays visitor/edit-builder mediated. | Rust-idiomatic under slice borrowing. | Users may desire arbitrary setters. | Cookbook and diagnostic coverage defeat the challenge. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 4 / REINVENT 5 / DISCARD 0.

## §12 Punch list

1. `restart/audit/pass-3-runtime/PASS-3.md:84-92` - Replace public `path!` wording with `pointer!` for the authored Rust macro, keep `PathPlan` as internal runtime type, and update §9 KEEP summary accordingly. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: path/select DSL. Lanes: 1, 5, 7, 9.

2. `restart/audit/pass-3-runtime/PASS-3.md:88-90`, `PASS-3.md:244-270`, `agent-2-path-select-dsl-designer.md:24`, `agent-6-ecosystem-architect.md:115-143` - Rename `bbnf-path-core`, `bbnf-path`, `bbnf-path-ts`, and `bbnf-test-fixtures` to `path-core`, `path`, `path-ts`, and `test-fixtures` in proposed crate trees and prose. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: workspace naming. Lanes: 1, 3, 5, 8, 9.

3. `restart/audit/pass-3-runtime/PASS-3.md:160-179` - Restructure `crates/bbnf/src/` to 4-10 immediate children, for example `lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/`; move tape/value/path/layout/host surfaces under those cohesive directories. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: Lock 13. Lanes: 1, 3, 9.

4. `restart/audit/pass-3-runtime/PASS-3.md:291-303` - Replace the benchmark table with exact rows: twitter M1 Pro `<= 380 us` vs sonic-rs `436 us` / simd-json `424 us`; canada M1 Pro `<= 2.8 ms` vs sonic-rs `3.144 ms` / simd-json `3.226 ms`; citm M1 Pro `<= 750 us` vs sonic-rs `854 us` / simd-json `831 us`; bootstrap `<= 3.0 ms` vs lightningcss `4.16 ms`; animate `<= 1.6 ms` vs lightningcss `1.97 ms`; simdjson On-Demand sustained `>= 5 GB/s M-series`, `>= 7 GB/s x86` vs `7 GB/s Intel Skylake`. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: SOTA gates. Lanes: 1, 4, 9.

5. `restart/audit/pass-3-runtime/PASS-3.md:291-303` - Add BBNF self-host as a non-Lock-8 internal gate: `< 100 ms full self-parse + format roundtrip`, explicitly not a SOTA peer claim. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: bench gates. Lanes: 4.

6. `restart/audit/pass-3-runtime/PASS-3.md:360-367` - Replace the unresolved punch-list with receiver/blocker/gate triples for every carry: PASS-1 tape ABI, PASS-2 metadata schema, SYNTHESIS input normalization, host escape policy, incremental fallback reporting. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: carry discipline. Lanes: 3, 8.

7. `restart/audit/pass-3-runtime/PASS-3.md:360-367`; after §6 or §7 - Add the `yaml.bbnf` future-grammar onboarding test: one grammar source file plus one `[workspace.metadata.bbnf.grammars.yaml]` block; zero Rust edits; generated value API, `pointer!`, `select!`, visitor, fixtures, and bench manifest appear from metadata/codegen. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: Lock 14. Lanes: 1, 5, 7.

8. `restart/audit/pass-3-runtime/PASS-3.md:42-80`, `PASS-3.md:333-338` - Add a per-X table for all nine extant grammars plus yaml, with columns for typed root, `ValueRef`, visitor, path schema, fixture manifest, host-fn route, and declaration-crate status. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: grammar generalisation. Lanes: 5.

9. `restart/audit/pass-3-runtime/PASS-3.md:139`; `agent-5-error-recovery-incremental-parsing.md:13-16` - Fold standalone `@recover` into `@error(recover)` or label it a compatibility alias with no separate grammar-level surface. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: BBNF extension discipline. Lanes: 3, 5, 9.

10. `restart/audit/pass-3-runtime/PASS-3.md:303`; §6 diagnostics ledger - Add verbatim diagnostics: `BBNF-LIFE001`, `BBNF-LIFE002`, `BBNF-LAYOUT002`, `BBNF-OPT001`, `BBNF-OPT002`, `BBNF-GRAMMAR001`, and updated `BBNF-POINTER00x` messages. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: friction forecast. Lanes: 1, 7.

11. `restart/audit/pass-3-runtime/PASS-3.md:291-303`; new generated budget subsection - Add generated LOC budgets: visitor emitted LOC by grammar, path-schema metadata Rust/sidecar byte budgets, tape identity field/method delta, and `cargo xtask regen --check` wall budget. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: generated-code budget. Lanes: 6.

12. `restart/audit/pass-3-runtime/PASS-3.md:305-319` - Add explicit PASS-2 gate that Backend IR is the only emission contract for typed roots, visitors, metadata, and path schema, and explicit Lock 3 gate that eager empty-path parsing elides cursor calls while lazy byte-skip remains inside the same parse implementation. Source verdict: REINVENT. Owner: PASS-3 amendment. Scope: cross-pass contracts. Lanes: 1, 3, 8.

Punch-list size: **12**.

## §13 Final readiness

> **Decision: amendment-required**
>
> PASS-3 is directionally sound and does not need re-drafting. The tape/direct union, slice-borrow constructors, visitor-mediated mutation, metadata-driven path/select validation, snapshot-aware incremental model, and no-default-declaration-crate posture survive challenge. The target cannot advance unchanged because its public path macro name, crate names, SOTA gates, Lock 13 tree, Lock 14 proof, generated budget, and carry gates are incomplete or stale. These are bounded amendments against PASS-3 text, not architectural reversals.
>
> Hereupon dispatch a PASS-3 amendment agent for the twelve-item punch list before SYNTHESIS consumes the PASS-3 output.
