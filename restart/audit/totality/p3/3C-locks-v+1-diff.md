---
agent: 3C
pass: T-P3-synthesis
cycle: V2
generated_at: 2026-05-21T19:37:16Z
v1_surface_targeted: restart/locks/LOCKS.md
diff_status: proposed-only
---

# 3C Proposed LOCKS v+1 Diff

This document is a line-level proposed diff only. T-P3 must not edit `restart/locks/LOCKS.md`; Pass Omega CRUD applies ratified edits after G3 and G-Omega. Evidence: `restart/prompts/totality/PASS-3-SYNTHESIS.md:21`-`24`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:189`-`198`, `restart/prompts/ORCHESTRATOR.md:165`-`170`, `restart/HANDOFF.md:44`-`47`.

V2 keeps the line-level diff text stable and pairs it with the cost/disposition ledger in `3C-locks-crystallisation.md`; ACCEPT/MODIFY in this diff remains a lock-text disposition only, never an implementation admission.

No lock is renumbered. The 16-lock count stays fixed. Any new lock, lock retirement, new directive, new BIR variant, public substrate API, retained sidecar, or `BackendShape` expansion remains user/G-Omega gated. Evidence: `restart/prompts/totality/PASS-3-SYNTHESIS.md:210`-`215`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md:57`-`58`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:223`-`230`.

## Hunk 1 - Supersede Scoped SK-V9 Allowance History

Target: replace `restart/locks/LOCKS.md:1`-`17`.

```diff
-# SK-V9 Scoped Lock 14 Allowances
+# Scoped Lock 14 Allowance History

-- `sk-v9-real-typed-w1` permits the SK-V9 W1 parent diff only for the
-  Apache/CITM measured typed row-table admission. The scoped owner paths are
-  `skinny/crates/bbnf-bench/src/report.rs`,
-  `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/RESULTS.md`,
-  `skinny/crates/bbnf-bench/target/skv9-w1/criterion/` (out-of-band capture),
-  `skinny/REDRESS.md`, `restart/skinny/tranches/sk-v9/HANDOFF.md`, and this
-  `restart/locks/LOCKS.md` allowance text.
-- The allowance does not authorize grammar, runtime, codegen, SIMD, fixture,
-  direct-output, generated typed-output, or generic-crate behavior changes.
-  `canada/real_typed_struct` remains blocked pending the full-fixture
-  DirectBuild-vs-serde checksum proof.
-- Evidence: `skinny/RESULTS.md` run id
-  `sk-v9-open:criterion-fnv64-a1e8a51ae806d386`, Apache
-  `real_typed_struct A / GO` at 8174 Mbps, and CITM
-  `real_typed_struct A / GO` at 35102 Mbps.
+- `sk-v9-real-typed-w1` remains a historical, scoped allowance for the
+  Apache/CITM measured typed row-table admission only. It does not authorize
+  grammar, runtime, codegen, SIMD, fixture, direct-output, generated
+  typed-output, or generic-crate behavior changes.
+- SK-V12 supersedes the allowance surface for generated non-JSON evidence:
+  `css_l4/declaration_values/direct_to_struct/main` is admitted as a
+  same-plane fact-stream row, with `lock14=pass:lock14_baseline::validate`,
+  but it is not full CSS parity, universal grammar closure, or a generic-crate
+  exception. Evidence: `skinny/RESULTS.md:94`, `skinny/REDRESS.md:3824`-`3840`.
+- All new generated non-JSON allowances are governed by Lock 14's generated
+  output rule below and remain G-Omega/Pass Omega edits, not T-P3 direct edits.
```

Candidate coverage: LAC-1E-11. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:110`, `restart/locks/LOCKS.md:1`-`17`, `restart/locks/LOCKS.md:78`.

## Hunk 2 - Lock 1 Substrate-Ceiling, Fact-Stream, And Union History

Target: append to current Lock 1 paragraph after `restart/locks/LOCKS.md:52`.

```diff
+    **2026-05-21 v+1 substrate-ceiling fold**: Skinny Track 2 remains a
+    substrate-ceiling probe, not a second substrate. Track 2 measures whether
+    the same `runtime::tape` + `bbnf-simd` APIs can reach the SOTA envelope
+    when hand-coded against the APIs codegen will emit; it does not authorize
+    hidden runtime identity, parser-owned sidecars, or a parallel representation.
+    Evidence: `restart/skinny/BENCH.md:71`-`107`,
+    `restart/skinny/BENCH.md:121`-`136`,
+    `restart/audit/totality/p1/1C-runtime-evidence.md:91`.
+
+    Lazy-offset tape with sparse flags is admitted as scoped JSON evidence
+    under this tape/direct union. It proves an offset-tape/direct shape can be
+    viable for the measured JSON lane, not that Lock 1 is universally closed
+    for every grammar or backend. Evidence: `skinny/REDRESS.md:246`-`256`,
+    `skinny/RESULTS.md:98`-`144`,
+    `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:45`-`46`.
+
+    Fact streams are output-plane contracts, not retained internal sidecars.
+    A generated fact stream such as `css_l4_declaration_value_fact_stream` may
+    be admitted only with strict comparator/oracle provenance and gate-consumed
+    telemetry; it does not by itself close a retained runtime substrate claim.
+    Evidence: `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:96`-`98`,
+    `skinny/RESULTS.md:94`, `restart/audit/totality/p1/1A-substrate-evidence.md:45`-`46`.
+
+    Every e-graph candidate, backend rewrite, imported scanner plan, union
+    candidate, and SIMD consumer must declare `substrate_target`,
+    `retention_lifetime`, and `policy_owner`. Allowed targets are
+    `local_temp_only`, `existing_tape`, `direct_sink`, and
+    `admitted_fact_output`; allowed lifetimes are `local_loop`,
+    `generated_function`, and `output_row`; allowed owners are
+    `generated_grammar`, `caller_data`, and `none`. Any retained class/mask
+    stream, parser-owned cursor/list state, public substrate API, `UnionTape`,
+    or second tape is rejected unless G-Omega explicitly amends Lock 1.
+    Evidence: `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:77`-`98`.
+
+    REDRESS 96/97/98 are binding substrate-ceiling history. Full class-column
+    vectors, streaming structural cursors, class-lane-only replays,
+    parser-owned sidecars, and `UnionTape`-style retained structures are not
+    shortlist-safe without a fresh material differential, scalar/checkasm or
+    equality proof, same-wave consumer, strict row gate, rollback path, and
+    abrogate threshold. Evidence: `skinny/REDRESS.md:2910`-`2940`,
+    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:120`-`137`.
```

Candidate coverage: LAC-1E-01, T2A-LAC-01, LAC-2B-05, LAC-2B-06, LAC-2D-05, LAC-2E-04. Must-include covered: Lock 1 substrate-ceiling history.

## Hunk 3 - Lock 2 Live-First Layout Wording

Target: append to current Lock 2 paragraph after `restart/locks/LOCKS.md:54`.

```diff
+    **v+1 live-state clarification**: `LayoutFacts.backend_shape` is the live
+    side-table evidence today; `Layout` and `LayoutSink` remain V1 public
+    API-freeze obligations unless Pass Omega removes those names from this
+    lock. A wave may not claim Lock 2 closure by pointing only to
+    `LayoutFacts` while the public `Layout` / `LayoutSink` names remain absent.
+    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:64`,
+    `restart/audit/totality/p1/1B-codegen-evidence.md:37`.
```

Candidate coverage: LAC-1E-02.

## Hunk 4 - Lock 3 Empty-Path Verification

Target: append to current Lock 3 paragraph after `restart/locks/LOCKS.md:56`.

```diff
+    **v+1 verification clause**: empty-path elision is not closed until a
+    generated-code unit/golden test proves the empty path emits no cursor calls
+    or equivalent consult symbols. Absence claims without captured command
+    output remain UNKNOWN verification actions, not lock closure.
+    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:65`,
+    `restart/audit/totality/p1/1E-locks-evidence.md:125`.
```

Candidate coverage: LAC-1E-03.

## Hunk 5 - Lock 8 Row-Plane Accounting And BENCH Section 8 Non-JSON Feed

Target: append to current Lock 8 paragraph after `restart/locks/LOCKS.md:66`.

```diff
+    **v+1 row-plane accounting**: SOTA is row-plane specific. JSON
+    `parse_only`, `direct_to_struct`, and `real_typed_struct` rows are separate
+    gates with same-plane strict comparators; CSS L4 declaration-values is a
+    SK-V12 `PASS-ADMIT` row on `css_l4_declaration_value_fact_stream`, not full
+    CSS parity, not universal grammar closure, and not SK-V13 close authority.
+    Evidence: `skinny/RESULTS.md:5`-`35`, `skinny/RESULTS.md:94`,
+    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:38`-`57`,
+    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95`-`110`.
+
+    Comparator-plane provenance is mandatory. Same-run Rust strict sonic rows
+    can anchor current JSON gates; simdjson, yyjson, asmjson, lightningcss, and
+    other sidecars are strict anchors only when the same corpus, output plane,
+    host, strictness, freshness, sidecar status, and gate-consumed artifact
+    provenance match the candidate row. Historical, absent, permissive,
+    x86-only, or different-plane rows are architecture pressure or comparator
+    notes, not gate anchors. Evidence: `restart/skinny/BENCH.md:678`-`684`,
+    `skinny/RESULTS.md:149`.
+
+    Non-JSON telemetry must feed the bench gate, not prose. A non-JSON row may
+    enter `skinny/RESULTS.md` only through the `BENCH.md` Section 8
+    post-bench gate
+    shape or a dedicated companion report consumed by that gate family, with
+    JSON guard proof when JSON rows can be affected. The legacy JSON
+    `gate --check-results` renderer alone is insufficient for an appended
+    non-JSON row. Evidence: `restart/skinny/BENCH.md:1498`-`1512`,
+    `restart/skinny/BENCH.md:1534`-`1545`,
+    `skinny/REDRESS.md:3836`-`3840`.
+
+    Direct digest hashing is a semantic-output contract. Byte-hash or SIMD
+    sub-hash acceleration is admissible only when Track 1, Track 2, serde, and
+    sonic strict equality hold for the same semantic output plane and no prior
+    A/GO guard silently demotes. Evidence:
+    `restart/audit/totality/p2/2F-parse-that-gaps.md:252`,
+    `restart/audit/totality/p1/1D-skinny-lessons.md:107`.
```

Candidate coverage: LAC-1E-04, T2A-LAC-02, LAC-2F-04. Must-include covered: non-JSON telemetry `BENCH.md` Section 8 feed.

## Hunk 6 - Lock 9 Runtime API Obligations

Target: append to current Lock 9 paragraph after `restart/locks/LOCKS.md:68`.

```diff
+    **v+1 skinny-scope clarification**: the skinny facade does not prove the
+    full Lock 9 surface. `parse_in(input, &bump)` and true generated owned
+    documents remain V1 runtime obligations until runtime API tests prove the
+    bump and owned surfaces share the same parse implementation and lifetime
+    discipline. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:71`,
+    `restart/audit/totality/p1/1E-locks-evidence.md:104`.
```

Candidate coverage: LAC-1E-05.

## Hunk 7 - Lock 10 Decision-Engine, Cost Evidence, And Five-Shape Fence

Target: append to current Lock 10 paragraph after `restart/locks/LOCKS.md:70`.

```diff
+    **v+1 decision-engine and cost-evidence clause**: the five
+    `BackendShape` variants remain the V1 search domain. A new
+    `BackendShape`, new directive, or new BIR variant is not admitted by cost
+    evidence and remains G-Omega gated. The current P1-P8 cascade and thin
+    `CostFacts` are diagnostics or compatibility evidence only until the
+    resolver generates backend-plan candidates, consumes generated grammar
+    facts, filters infeasible plans, records selected and rejected alternatives,
+    and extracts with active cost evidence. Evidence:
+    `restart/audit/totality/p1/1B-codegen-evidence.md:36`-`39`,
+    `restart/audit/totality/p2/2D-cost-model.md:188`-`190`.
+
+    Regex/HIR facts are required where regex or scanner plans influence
+    backend-shape or scanner selection. Opaque pattern strings alone cannot
+    justify SIMD, scanner-plan import, or backend-shape admission. Exact fact
+    schema belongs in `ARCHITECTURE.md`, but Lock 10 treats stale/static
+    fallback and opaque-string-only selection as non-admitting evidence.
+    Evidence: `restart/audit/totality/p2/2F-parse-that-gaps.md:251`,
+    `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:79`.
+
+    Decision-engine and SIMD/substrate candidates fail closed on e-graph cap,
+    CSP timeout, stale cost evidence over 30 percent, generated LOC overrun,
+    admitted-row regression, or any scalar/checkasm/equality failure.
+    Evidence: `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:103`-`115`.
```

Candidate coverage: T2A-LAC-05, LAC-2C-04, LAC-2D-01, LAC-2D-02, LAC-2D-03, LAC-2F-03.

## Hunk 8 - Locks 11 And 12 Workspace Drift

Target: append to current Lock 11 after `restart/locks/LOCKS.md:72` and current Lock 12 after `restart/locks/LOCKS.md:74`.

```diff
+    **Lock 11 v+1 workspace verification**: root legacy workspace drift is not
+    skinny truth. A.W0/A.W1 closure requires `cargo metadata` or equivalent
+    artifact proof that `ser`, `gorgeous`, `simd-scan`, `bbnf-path`, and
+    `bbnf-path-ts` match the archive/rename/removal state this lock names.
+    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:73`,
+    `restart/audit/totality/p1/1E-locks-evidence.md:105`.
+
+    **Lock 12 v+1 archive verification**: the archive ceremony remains a hard
+    precondition; root workspace membership after archive must be proven by a
+    committed metadata transcript or Pass Omega-equivalent evidence, not by
+    stale prose. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:74`,
+    `restart/audit/totality/p1/1E-locks-evidence.md:105`.
```

Candidate coverage: LAC-1E-06.

## Hunk 9 - Lock 13 Generated And Gate/Report Exceptions

Target: append to current Lock 13 paragraph after `restart/locks/LOCKS.md:76`.

```diff
+    **v+1 exception discipline**: generated files are exempt only when they are
+    rostered generated artifacts with per-wave generated-LOC budgets and
+    regeneration checks. Bench/report/gate files may exceed 500 LOC only under
+    an explicit gate-surface budget and committed LOC transcript. The 500 LOC
+    ceiling remains binding for non-generated production modules; directory
+    fanout is a violation only when the inventory proves mixed concerns, not
+    merely many cohesive ISA/test partitions. Evidence:
+    `skinny/REDRESS.md:299`-`312`,
+    `restart/audit/totality/p1/1F-anti-pattern.md:31`-`32`,
+    `restart/audit/totality/p1/1E-locks-evidence.md:106`.
```

Candidate coverage: LAC-1E-07.

## Hunk 10 - Lock 14 Generated Output, Per-Wave Gate, And Grammar-Policy Transfer

Target: append to current Lock 14 paragraph after `restart/locks/LOCKS.md:78`.

```diff
+    **v+1 generated-output allowance**: generated files under
+    `runtime/src/grammars/<name>/` may contain grammar names only when emitted
+    from the rostered generator using grammar source plus workspace metadata.
+    This allowance does not extend to hand-coded provider enums, root aliases,
+    generic-crate grammar branches, grammar-named public types in generic APIs,
+    tests/proof fixtures routed through generic roots, or grammar-shaped policy
+    mining. Evidence: `restart/audit/totality/p2/2C-grammar-neutrality.md:184`,
+    `restart/audit/totality/p1/1C-runtime-evidence.md:79`-`85`.
+
+    Generic crates consume generated provider manifests, generated
+    sink/fact/value/flag surfaces, and generated grammar facts. They may not
+    hand-code `RuntimeProvider::{Json, CssL4DeclarationValues}`, JSON/CSS
+    renderer branches, JSON punctuation alphabets, object/array/pair/string/
+    number/bool/null role mining, hardcoded sink callback names, or
+    grammar-specific feature flags. Evidence:
+    `restart/audit/totality/p1/1B-codegen-evidence.md:58`-`60`,
+    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:57`-`65`.
+
+    **Per-wave gate enforcement**: any wave touching generic crates, generated
+    provider manifests, primitive policy manifests, runtime roots, codegen
+    templates, decision-engine facts, or shared `bbnf-simd` consumers must run
+    a Lock 14 baseline gate plus a grammar-name and grammar-shape leak census
+    in the same wave. At minimum, the gate checks generated provider registry,
+    grammar-shape role mining, generated sink/fact/value/flag ownership,
+    primitive policy source, CSS plus Sheets/BBNF-self witness or negative
+    control when claiming fleet-wide transfer, and decision-engine generated
+    facts. Evidence:
+    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`75`,
+    `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:47`-`51`.
+
+    Shared `bbnf-simd`, parse-that, and future regex APIs expose
+    grammar-neutral facts and primitives only. Quote, escape, control,
+    delimiter, number, string, and no-string/no-number policy must come from
+    generated grammar config or caller data, not hardcoded JSON/CSS constants.
+    A primitive claimed grammar-neutral must exercise at least one non-JSON
+    consumer or record a measured deletion/rejection. Evidence:
+    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:226`-`230`,
+    `restart/audit/totality/p2/2C-grammar-neutrality.md:188`,
+    `restart/audit/totality/p2/2F-parse-that-gaps.md:249`.
```

Candidate coverage: LAC-1E-08, T2A-LAC-04, LAC-2B-03, LAC-2C-01, LAC-2C-02, LAC-2C-03, LAC-2C-05, LAC-2F-01. Must-include covered: Lock 14 per-wave gate enforcement.

## Hunk 11 - Lock 15 Skinny Versus Root Profile Scope

Target: append to current Lock 15 paragraph after `restart/locks/LOCKS.md:80`-`85`.

```diff
+    **v+1 scope clarification**: skinny release profile evidence proves skinny
+    enforcement only. Root workspace thin-LTO or profile drift remains a V1
+    migration gap until the root release build proves `lto=fat`,
+    `codegen-units=1`, panic policy, and debug-symbol requirements for every
+    generated runtime and throughput-sensitive consumer. JSON `parse_value_at`
+    i-cache evidence is scoped JSON evidence, not a blanket grammar closure.
+    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:77`,
+    `skinny/REDRESS.md:258`-`264`,
+    `restart/HANDOFF.md:132`-`134`.
```

Candidate coverage: LAC-1E-09.

## Hunk 12 - Lock 16 Manifest, Strict Checkasm, Escape Mask, Orphans, And Hardware Gates

Target: append to current Lock 16 paragraph after `restart/locks/LOCKS.md:112`.

```diff
+    **v+1 primitive manifest**: every `core::arch::*`, `target_feature`, and
+    `asm!` use-site in `bbnf-simd`, parse-that facades, generated scanners, or
+    collapsed-stage code maps to a manifest row containing stable primitive id,
+    abstract primitive name, primary ISA/library citation, hardware gate,
+    scalar reference, strict checkasm/parity command, corpus/equality parity,
+    grammar policy source, substrate target, retention lifetime, policy owner,
+    same-wave production consumer, expected row/feature gate, LOC/risk,
+    rollback path, abrogate threshold, and final disposition. Evidence:
+    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:150`-`180`,
+    `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:89`-`101`.
+
+    Admission checkasm commands run with `BBNF_SIMD_STRICT=1`. Non-strict
+    parity is exploratory only and cannot admit a primitive, route, or row.
+    Every scalar/checkasm/equality failure rejects the candidate for that wave.
+    Evidence: `skinny/REDRESS.md:3621`-`3625`,
+    `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:47`-`58`.
+
+    `escape_mask_64` is an admitted correctness prerequisite, not a production
+    SIMD/ASM throughput primitive. Its checkasm-backed state covers the
+    historical xorshift falsifier and scanner parity cases, but it admits a
+    row only when a JSON/CSS string or escape consumer wires it in the same
+    wave and moves or rejects the named row under strict comparator evidence.
+    Evidence: `skinny/REDRESS.md:3603`-`3632`,
+    `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:91`-`92`,
+    `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:49`.
+
+    At close, every source-present primitive is exactly one of `wired`,
+    `deleted`, `scalar-delegate-non-ASM`, or
+    `architectural-block-with-REDRESS`. `inventory_demoted_with_evidence` is
+    historical evidence only. Support-only hint modules, unconsumed prefix/next
+    bitmap bodies, cache hints without exact caller placement, and orphan
+    `asm!`/intrinsic files do not close Lock 16. Evidence:
+    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:194`-`206`,
+    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:84`-`93`.
+
+    `CollapsedStage` is admissible only as a concrete emitted transient
+    strategy with scalar reference, strict parity/checkasm, feature gate,
+    local temporary lifetime, and same-wave measured consumer. AVX-512
+    literature is x86 architecture pressure and cannot close M5/aarch64 rows.
+    Evidence: `restart/audit/totality/p2/2D-cost-model.md:191`,
+    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:223`-`230`.
+
+    Native `svmatch_u8` is SVE2-only. The existing Lock 16 NEON set-membership
+    row remains a separate NEON reduction-tree port only if the manifest names
+    the NEON implementation, scalar oracle, strict checkasm, hardware gate,
+    and consumer row. Evidence:
+    `restart/audit/totality/p2/2E-host-arch-esoterica.md:270`,
+    `restart/locks/LOCKS.md:95`.
+
+    PMULL/CSSC, union, ASM-gen, cache-hint, parse-that, and hardware facade
+    routes require material-differential text against prior REDRESS rows,
+    micro-prove-first evidence where applicable, grammar-policy proof, and a
+    same-wave production consumer or measured deletion/rejection. Evidence:
+    `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:120`-`137`,
+    `skinny/REDRESS.md:3766`-`3820`,
+    `skinny/REDRESS.md:3864`-`3868`.
```

Candidate coverage: LAC-1E-10, T2A-LAC-03, LAC-2B-01, LAC-2B-02, LAC-2B-04, LAC-2B-07, LAC-2D-04, LAC-2E-01, LAC-2E-02, LAC-2E-03, LAC-2F-02. Must-include covered: Lock 16 checkasm plus `escape_mask_64` amendment.

## Hunk 13 - G-Omega Boundary Footer

Target: add a non-numbered governance footer after current Lock 16 section, before `## Lanes` at `restart/locks/LOCKS.md:114`.

```diff
+## v+1 Governance Boundary
+
+The v+1 text above is proposed by T-P3 only. It is not active LOCKS text until
+G3 accepts the T-P3 packet, Pass Omega CHALLENGE converges, and G-Omega
+authorizes CRUD operations on governance surfaces. No implementation wave may
+use proposed v+1 wording as permission to edit source, write RESULTS/REDRESS,
+add a directive, add a BIR variant, add or retire a lock, expand
+`BackendShape`, add a public substrate API, retain a sidecar, or dispatch
+SK-V13 Wave 0 before G-Omega closes. Evidence:
+`restart/prompts/totality/PASS-3-SYNTHESIS.md:179`-`198`,
+`restart/prompts/ORCHESTRATOR.md:165`-`170`,
+`restart/skinny/tranches/sk-v13/SYNTHESIS.md:112`-`122`.
```

Candidate coverage: cross-cutting G-Omega boundary from PASS-3/T-P2/SK-V13. Must-include covered: G-Omega gating boundaries.
