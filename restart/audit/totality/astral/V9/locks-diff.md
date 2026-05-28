# Pass Omega V9 Locks Diff

Disposition: proposed amendment, G-Omega gated.

SK-V15 totality requires one `restart/locks/LOCKS.md` addendum before the
existing `## v+1 Governance Boundary`. The diff preserves the 16 numbered
locks, preserves the five `BackendShape` variants, and adds no directive, BIR
variant, substrate, public substrate API, retained sidecar, lock, lock
retirement, or sixth shape. `FactStream` remains outside `BackendShape`.
Admission evidence remains Apple M5 Max/aarch64 only; x86/AVX-512 evidence is
diagnostic only.

Verification:

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
# 16

find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
# 67
```

Five-shape canon:

```text
EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage
```

`skinny/crates/ir/src/lib.rs:340`-`345` defines those five variants, and
`skinny/crates/ir/src/cost.rs:333`-`339` returns `[BackendShape; 5]`.

The proposed diff extracts and applies cleanly with:

```sh
awk '/^diff --git/{flag=1} flag && $0 != "```"{print}' restart/audit/totality/astral/V9/locks-diff.md | git apply --check -
```

## Proposed Unified Diff

```diff
diff --git a/restart/locks/LOCKS.md b/restart/locks/LOCKS.md
--- a/restart/locks/LOCKS.md
+++ b/restart/locks/LOCKS.md
@@ -581,3 +581,32 @@
+## SK-V15 T-P3 v+1 Crystallisation Addendum
+
+This addendum is the SK-V15 T-P3 3C candidate fold. It preserves the 16 numbered locks and the exact five BackendShape variants `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`; it adds no directive, BIR variant, substrate, public substrate API, retained sidecar, lock, lock retirement, or sixth shape. T-P1 is a clean-final/G1-auto-pinned input, not a normal two-clean-cycle lock; T-P2 is the normal Section 3Z locked research input. Evidence: `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:21`-`28`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:15`-`19`, `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:70`-`74`.
+
+- Lock 1 substrate/fact-stream/sidecar clause: `FactStream` remains an output-plane/admitted-product category only, not a sixth BackendShape and not a retained internal sidecar. CSS fact streams require typed schema/provenance and gate-consumed telemetry; string-only fact streams, retained cursor/list/class-column/sidecar, parser-owned structural streams, public `UnionTape`, second tape, runtime regex/DFA substrate, or cross-call classifier state remain rejected unless a later G-Omega explicitly amends Lock 1. Runtime regex/DFA manifest plus consumer proof is necessary but never sufficient before that amendment. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:130`, `restart/audit/totality/p2/2A-sota-landscape.md:110`, `restart/audit/totality/p2/2C-grammar-neutrality.md:145`, `restart/audit/totality/p2/2F-parse-that-gaps.md:120`.
+
+- Lock 2 live-state clause: `LayoutFacts.backend_shape` is current implementation evidence, but it does not close Lock 2 while the public `passes::layout`, `Layout`, and `LayoutSink` obligations remain absent. A future closure must either land those names or have Pass Omega narrow the lock text. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:131`, `restart/audit/totality/p1/1E-locks-evidence.md:91`.
+
+- Lock 3 empty-path clause: cursor-elision closure requires generated-code golden evidence or a unit test proving the empty path emits no cursor-consult symbols. A missing `__EAGER_EMPTY_PATH` grep hit is an UNKNOWN, not closure. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:132`, `restart/audit/totality/p1/1E-locks-evidence.md:122`.
+
+- Lock 4/6 solver-bridge clause: egraph/CSP composition may close only after the dependency graph either removes the direct `egraph -> csp-solver` dependency or records it as an explicit accepted exception with bridge ownership. Silent drift is not closure evidence. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:133`, `restart/audit/totality/p1/1E-locks-evidence.md:120`.
+
+- Lock 6/14 generated-output and deletion clause: generated files require line-1 provenance, a rostered generator, byte-equivalent non-writing regen/check proof, and a same-wave replacement provider before deletion or retirement. Header-only proof, fake `@generated` templates, `CSS_GENERATED_RS` centralization, and provider/template deletion before W5/W6 typed replacement proof reject. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:134`, `restart/audit/totality/p1/1E-locks-evidence.md:144`, `restart/audit/totality/p2/2C-grammar-neutrality.md:147`, `restart/skinny/tranches/sk-v15/SPEC.md:192`-`204`.
+
+- Lock 8 row-plane/broadcast clause: every SOTA or row-movement claim declares `value_plane`, comparator workload, command, input, equality, timing, host, measurement origin, `measurement_row_id`, and `broadcast_group_id`. Cross-plane comparisons are diagnostic only. Repeated throughput tuples across conceptual row IDs are non-admit unless each row has independent command/input/equality/timing. CSS close requires generated typed value/document/view/visitor output and same-workload `cssparser` equality before `lightningcss` CSSOM/value pressure can admit. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:136`-`138`, `restart/audit/totality/p2/2A-sota-landscape.md:107`-`110`, `restart/audit/totality/p2/2C-grammar-neutrality.md:146`, `restart/audit/totality/p2/2F-parse-that-gaps.md:122`.
+
+- Lock 9 API-surface clause: borrowed/Cow JSON evidence is partial only. `parse_in(input, &bump)` and `parse_owned(input)` remain open V1 obligations until generated API tests prove they share the same parser and lifetime discipline. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:138`, `restart/audit/totality/p1/1E-locks-evidence.md:98`.
+
+- Lock 10 decision/five-shape clause: Decision Engine closure requires at least one asserted egraph rewrite with nonzero work, cost extraction from measurement-bearing row-local facts, nonzero or explicitly rejected candidate `perf_cost`, a non-tautological CSP whose fact removal or alteration changes SAT/UNSAT or selection, grammar-neutral facts, generated selection/rejection consumed by a gate, real lowerer output or gate-consumed rejection for all five BackendShape variants, and an all-five gate over exactly `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`. Adding a sixth shape, new directive, or new BIR variant is not a fix and remains G-Omega gated. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:139`, `restart/audit/totality/p2/2D-cost-model.md:114`-`118`, `skinny/crates/ir/src/lib.rs:339`-`345`, `skinny/crates/ir/src/cost.rs:333`-`339`.
+
+- Lock 11/12 topology/archive clause: root workspace topology and archive ceremony are not closed by skinny evidence. Closure requires explicit root evidence for `crates/path`, `crates/path-core`, `crates/parse-that`, removal or preservation routing for legacy crate names, and an archive inventory where required. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:135`, `restart/audit/totality/p1/1E-locks-evidence.md:96`-`101`.
+
+- Lock 14 grammar-generalisation clause: generic codegen may consume generated provider manifests, generated grammar facts, and generated sink/fact/value surfaces, but may not hand-code `RuntimeGenerationMode`, profile arrays, CSS profile matches, JSON/CSS runtime families, JSON punctuation or role mining, generic grammar switches, or generic-crate grammar branches. Lock 14 gates report included roots, excluded roots, owner, reason, self-scan status, primitive status, gate consumer, affected rows, and disposition. Future grammar onboarding is source/metadata-only through CSS plus Sheets or BBNF-self witnesses; no new directive, BIR variant, sixth BackendShape, or generic code branch is admissible. CSS value parsing may reuse byte kernels below a CSS scalar oracle, but JSON string/number semantic APIs are not the CSS parser. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:140`-`141`, `restart/audit/totality/p2/2C-grammar-neutrality.md:144`, `restart/audit/totality/p2/2C-grammar-neutrality.md:148`-`149`, `restart/audit/totality/p2/2F-parse-that-gaps.md:121`.
+
+- Lock 15 profile-scope clause: skinny release-profile compliance is scoped skinny evidence only. Root release profile, generated runtimes, throughput-sensitive consumers, and `target-cpu=native` host-bound rows require separate proof before Lock 15 closure. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:142`, `restart/audit/totality/p1/1E-locks-evidence.md:104`.
+
+- Lock 16 primitive-manifest clause: every primitive or parse-that-family route records owner (`parse-that-regex`, `bbnf-simd`, or generated provider), scalar oracle, strict parity/checkasm command, Apple M5 Max/aarch64 hardware gate or explicit fallback, same-wave consumer, row movement target, lock16 status, fallback state, LOC/risk/wave owner/hard-cap fit, rollback/abrogate rule, and final disposition. Legacy `skinny/crates/bbnf-regex` is a temporary path awaiting Lock 11 rename cleanup, not an admissible future owner or peer owner. `scalar-delegated` is an admissible fallback only when no SIMD row movement is claimed; source inventory and `bbnf.asm` macro names are not admission. Non-strict parity is exploratory. PMU counters support close only with row-local command/input/equality/timing and no broadcast group. SVE/SVE2 primitives must not be filed as NEON/AdvSIMD; future `svmatch_u8` requires an SVE2 host and scalable-vector dispatch. PMULL/CSSC, CollapsedStage, DotProd/I8MM, ternary bitwise, and CSS semantic reuse require the same manifest and consumer proof before admission. Runtime regex/DFA manifest and consumer proof are necessary but never sufficient; any runtime regex/DFA substrate requires prior G-Omega amendment to Lock 1 before Lock 16 admission can proceed. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:143`, `restart/audit/totality/p2/2A-sota-landscape.md:111`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:201`-`204`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:139`-`141`, `restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`122`.
+
+
 ## v+1 Governance Boundary

 The v+1 text above is active only because Pass Omega CHALLENGE converged and
```
