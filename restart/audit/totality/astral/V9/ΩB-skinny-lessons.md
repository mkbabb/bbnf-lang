# Omega-B Skinny Lessons - Pass Omega V9

Date: 2026-05-28.
Worker: Pass Omega V9 Omega-B skinny-lessons.
Scope: SK-V14 close, PASS-IMPL V1 overfit audit, SK-V15 Alpha/S-P0/S-P1/S-P2/S-P3 packets, `skinny/RESULTS.md`, `skinny/REDRESS.md`, and T-P1/T-P2/T-P3 hardening consolidated packets.
Disposition: ACCEPT as proposal-only digest. No live V1 surface is edited here.

## Executive Lesson

SK-V14's ledger close is bifurcated: JSON is honest, scoped proof-of-concept evidence; CSS L4 is contrived admission evidence and must not be carried into V1 as a SOTA or grammar-neutral win. PASS-IMPL V1 makes this distinction explicit: JSON hardcoding is acceptable as a proof-of-concept, while CSS L4 has a dispositive 24-row broadcast, a brace-counter/wrong-plane comparator, and a hand-written string-literal tokenizer pretending to be generated output (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:8`, `:21-33`).

SK-V15 is therefore not a normal "next optimisation" tranche. Its locked S-P3 packet is a W0-W11 prune-then-rebuild contract: preserve JSON 51/51 as a guard baseline, demote CSS W8R evidence, restore Lock 14/16 gates, repair Pattern H provenance, activate the Decision Engine, quarantine FNV bench products, and close only through PASS-IMPL V2 or row-level intrinsic-block proof (`restart/skinny/tranches/sk-v15/SPEC.md:49-84`, `:172-204`).

The V1 surfaces must absorb the lesson as status and gate discipline, not as implementation closure. The inflection point remains future-tense: JSON is at the inflection point; CSS, Pattern H, Lock 14/16, and Decision Engine are not.

## Evidence Classification

| Evidence class | Status | Why it matters for V1 |
|---|---|---|
| JSON parse_only / direct_to_struct / real_typed_struct | Honest guard evidence. SK-V14 records 17/17 rows in each JSON family, strict same-plane, measured on native Apple M5 Max / aarch64. S-P0 preserves this as the clean baseline, with only the W11L/W11N/W11O bench-only FNV caveat. | V1 may cite JSON as a scoped proof-of-concept for parse/value/SOTA, but must not generalize it to CSS or all grammars. |
| CSS L4 W8R 24-row admit | Contrived. One aggregate tuple is projected into 24 conceptual rows; `cssparser` is faster than Track 1 on the same aggregate tuple; lightningcss compares against a CSSOM plane Track 1 does not emit. | V1 must demote current CSS rows to diagnostic or aggregate evidence until W5/W6 typed CSS proof and same-workload retime land. |
| CSS equality/parity | Fake parity. The CSS gate checks marker strings and summary/fact-stream shape, not semantic equality between Track 1 and comparator value output. | V1 gate language must require typed CSS value/document equality, not marker acceptance or `wrong_plane_outputs=0` strings. |
| CSS generator claim | Contrived. The seven CSS generated bodies are byte-identical copies of `CSS_GENERATED_RS`, a 646-line hand-written tokenizer string literal, not grammar-derived emission. | V1 generated-output claims must require generator provenance and non-writing regen/check proof before provider/template retirement. |
| Pattern H | Open. The root runtime census remains 67 files, and 0/67 carry line-1 generated provenance; partial template success on 5/9 grammars does not close the category. | V1 implementation-status must state "67 current repair surface, 0/67 generated provenance" until W4 proves all 67. |
| Lock 14 / Lock 16 gates | Open. Lock 14 scan roots omit known leak-bearing files; Lock 16 lacks a full source-present primitive manifest with strict parity/checkasm status. | V1 lock text must require included roots, excluded roots, self-scan status, primitive status, gate consumer, affected rows, and disposition. |
| Decision Engine | Scaffold. Zero e-graph rewrites, tautological CSP, grammar-named facts, and four label-string BackendShape lowerers are not load-bearing. | V1 must treat Decision Engine as non-admitting until W7-W9 produce executable selection/lowering evidence while preserving exactly five BackendShape variants. |
| FNV closed-enum products | Bench-only quarantine. W11L/W11N/W11O strict-product rows have a closed-enum/FNV coupling risk but are isolated to bench/xtask code. | V1 must forbid production FNV arbiters or hash correctness proof; W10 owns production-root scans and adversarial equality fixtures. |
| SIMD / primitives | Native aarch64 only. x86 and AVX-512 rows are diagnostic; source-present primitives need scalar oracle, strict parity/checkasm, same-wave consumer, and row movement. | V1 Lock 16 and MASTER-PLAN must reject wrong-host close evidence and source-only primitive claims. |

## Longitudinal Lesson Table

| Cycle / packet | Load-bearing win | Load-bearing rejection | Trajectory correction for V1 |
|---|---|---|---|
| V5 W5R / REDRESS-209 | Provider deletion was correctly identified as requiring rebuild capability. | Static centralization only renamed the old provider mesh. | V1 delete/retire rules must require provider proof no later than the delete wave. |
| V6 W5BR / REDRESS-210 | Request-boundary source consumption became a separate validated step. | Request source consumption did not mean runtime bytes were provider-free. | V1 must distinguish source ingestion, frontend lowering, generator body, and provider deletion. |
| V7 W5B-GENR / REDRESS-211 | Generator body sequencing was tightened. | The generator could not consume grammar constructs before frontend/import/IR closure existed. | V1 must block generator claims when the frontend cannot parse, import-resolve, lower, and carry constructs into IR. |
| V8 W5B-FRONTENDR / REDRESS-212 | Cap authority became explicit. | Internal sub-slices could not satisfy the one-wave hard cap without formal SPEC/Omega wave boundaries. | V1 wave plans must expose capped sub-waves, aggregate close semantics, and no challenge-time overflow. |
| SK-V14 close ledger | JSON closes 51/51 as strict same-plane guard evidence. | CSS 24/24 close is audit-falsified; generic infrastructure remains mixed. | V1 implementation status must split JSON "validated guard" from CSS "diagnostic/open". |
| PASS-IMPL V1 | It cleanly classifies honest JSON, clean primitives, and bench-only FNV caveat. | CSS broadcast, fake parity, string-literal tokenizer, Pattern H 67/0, Lock 14 holes, Decision scaffold. | V1 locks and master plan need explicit anti-broadcast, anti-fake-generated, gate-exclusion, and scaffold-rejection clauses. |
| SK-V15 Alpha | The receiver set A-G correctly brackets the repair: CSS, gates, codegen, Pattern H, Decision, FNV. | Alpha hardening found six initial REVISEs, showing the initial packet needed aarch64, dependency-table, cost, executable-evidence, and exclusion-gate folds. | V1 should carry the folded constraints, especially Apple M5 Max / aarch64 and dependency-row discipline. |
| SK-V15 S-P0 | Overfit audit re-confirmed JSON as guard baseline and identified exact prune/rebuild receivers. | S-P0 does not clear: CSS, gates, codegen, Pattern H, and Decision remain failed or prune-required. | V1 must not present SK-V15 as implemented; it is a locked implementation contract after G-Omega, not landed behavior. |
| SK-V15 S-P1 | Fresh profiles/PMU evidence update JSON hot-leaf analysis without mutating RESULTS. | P1 does not rescue CSS and does not reopen REDRESS-blocked sidecar/tiny-string/numeric routes. | V1 should treat profile/PMU rows as research evidence, not admission rows unless a wave consumes them. |
| SK-V15 S-P2 | Grammar-neutral survivor families are bounded: byte-set/classifier, string/literal, UTF-8, escape segments, same-tape operations, direct cursor/FIRST-set templates. | Numeric/digit, PMULL hot-body, CSSC bulk, x86/AVX-512, retained sidecars, schema-shaped builders, and CSS broadcast evidence remain rejected or diagnostic. | V1 should require scalar references, parity/checkasm, same-wave consumers, and CSS/Sheets/BBNF-self witnesses for generic claims. |
| SK-V15 S-P3 | W0-W11 is locked with dependency rows, telemetry, non-JSON receiver matrix, no-W12 cap discipline, and no-orphan close. | S-P3 is proposal-only; no implementation wave dispatches before Pass Omega/G-Omega. | V1 surfaces must route implementation state through W0-W11 and preserve G-Omega as the next mandatory gate. |
| T-P1 hardening V5 | T-P1 closes all known evidence defects and preserves FNV as quarantine telemetry. | It is clean-final/G1-auto-pinned, not normal two-clean-cycle §3Z, and it deliberately leaves CSS, Pattern H, gates, Decision, and FNV open. | V1 governance notes must not pretend normal §3Z lock; open work stays visible. |
| T-P2 hardening V3 | T-P2 normal §3Z lock confirms research conclusions: JSON honest but scoped; CSS refuted; Lock 14/16, Decision, and primitive gates require executable proof. | Runtime regex/DFA import, self-excluding gates, wrong-host close, CSS substitution, and sixth-shape paths remain blocked. | V1 should fold T-P2 as the research authority for receiver/gate requirements. |
| T-P3 hardening V5 | T-P3 final convergence accepts proposal-only skinny fold after V4 citation repair; 16-lock count and five-shape canon are preserved. | It is not an untouched two-clean-cycle lock and does not edit live surfaces. | Pass Omega V9 may propose V1 surface updates, but CRUD waits for G-Omega. |

## Specific V1 Surface Update Needs

| V1 surface | Required update need | Evidence driver |
|---|---|---|
| `restart/ARCHITECTURE.md` | Add or repair implementation-status wording: JSON is scoped guard evidence; CSS is diagnostic/open; Pattern H is 67 files with 0/67 generated provenance; Decision Engine is scaffold; SIMD/primitive claims are Apple M5 Max / aarch64 only. | PASS-IMPL V1 headline and inflection table; SK-V15 SPEC close conditions. |
| `restart/MASTER-PLAN.md` | Reconcile SK-V15 as W0-W11 PRUNE-before-REBUILD: W0 baseline, W1 CSS demotion, W2 gates, W3 codegen leaks, W4 Pattern H, W5/W6 CSS typed provider/retime, W7-W9 Decision/lowerers, W10 FNV, W11 PASS-IMPL V2. State no W12 or challenge-time overflow. | SK-V15 SPEC wave manifest and S-P3 hardening V4. |
| `restart/locks/LOCKS.md` | Strengthen Lock 14/16 and related lock prose: anti-broadcast admission, fake parity rejection, generated provenance, full scan inclusion/exclusion report, source-present primitive manifest, wrong-host exclusion, and FNV production quarantine. Preserve 16 locks and the five BackendShape canon. | PASS-IMPL V1 gate holes; SK-V15 S-P0; T-P2/T-P3 hardening. |
| `restart/HANDOFF.md` | Record governance: T-P1 is clean-final/G1-auto-pinned-not-normal-§3Z; T-P2 is normal §3Z locked; T-P3 is final all-ACCEPT after citation-only repair; G-Omega is the next mandatory gate before CRUD or SK-V15 W0. | T-P1 V5, T-P2 V3, T-P3 V5 consolidated packets. |
| `restart/MIGRATION.md` | Carry delete-before-provider and retire-before-rebuild rules for CSS provider/template, `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream `parse()`, brace-counter proof, Pattern H provenance, Decision scaffolds, and FNV production migration. | SK-V15 dependency rows `DEP-W1` through `DEP-W11`. |
| `restart/skinny/BENCH.md` | Mark CSS W8R rows as diagnostic negative fixtures unless W1 demotes/collapses them and W6 retimes typed CSS against cssparser. Add anti-broadcast fields as gate-consumed, not producer-only telemetry. | PASS-IMPL CSS broadcast; SK-V15 telemetry fields. |
| `restart/skinny/COMPILER.md` | Require grammar-derived generation, non-writing regen/check proof, no JSON/CSS runtime mode split, no static CSS profile roster, and non-JSON receivers for generic surfaces. | PASS-IMPL string-literal tokenizer; SK-V15 W3/W4 and receiver matrix. |
| `restart/skinny/HARDENING.md` | Add CH3 wave-graph cycle detection, CH5 broadcast detection, and CH7 gate-exclusion detection as recurring hardening lenses. | PASS-IMPL procedural addenda and S-P3 hardening. |
| `restart/skinny/INDEX.md` | Reclassify SK-V14 CSS close as audit-demoted and SK-V15 as locked planning/dispatch contract, not implementation landing. | SK-V15 HANDOFF/SPEC dispatch lock. |
| `restart/skinny/SUBSTRATE.md` | Preserve Lock 1 lesson: transient masks and local classifiers may write existing tape/sink/fact output, but retained sidecars, cursor/list/class streams, public `UnionTape`, alternate document projection, second tape, or sixth BackendShape remain blocked. | S-P2 survivor boundary and SK-V15 forbidden vocabulary. |
| `restart/skinny/WORKSPACE.md` | Reflect native-host gate discipline: admission and SIMD/primitive close claims are Apple M5 Max / aarch64; x86/AVX-512 are diagnostics only. | SK-V15 non-negotiables and T-P2 locked research surface. |

## V1 Fold Requirements By Topic

### JSON Guard

V1 should say: JSON parse/value/SOTA evidence is real and guarded, but scoped. All 51 JSON rows remain strict same-plane rows, and any JSON-adjacent generator/codegen change in SK-V15 must rerun and preserve the guard. V1 should not use JSON as proof of CSS, Sheets, BBNF-self, or generic fleet closure.

### CSS Contrivance

V1 should say: CSS L4 current ledger evidence is diagnostic. The 24 CSS rows cannot remain individual admits unless each has independent typed-output measurement. The W8R tuple (`track1=2319.041`, `cssparser=2362.037`, `lightningcss=929.281`) is a negative fixture: it shows cssparser beating Track 1 on the aggregate and lightningcss living on a different output plane. A valid CSS close requires typed value/document/view/visitor output, same-workload cssparser retime, and strict typed equality.

### Generated Output And Pattern H

V1 should say: generated status requires line-1 provenance plus a non-writing regen/check route. Header-only changes, byte-identical replay of a hand-written string, or destructive deletion without same-wave replacement proof fail. The current Pattern H truth is 67 root files, 0 generated headers, partial template success only.

### Gates And Primitives

V1 should say: Lock 14/16 gates are evidence only when they scan or report their exclusions. Any gate must print included roots, excluded roots, reasons, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition. Any primitive/SIMD/ASM helper must have scalar oracle/reference, strict parity/checkasm where relevant, same-wave consumer, and native aarch64 admission evidence. x86/AVX-512 is diagnostic.

### Decision Engine And BackendShape

V1 should say: Decision Engine is not load-bearing until W7-W9 prove one or more real e-graph rewrites, non-tautological CSP, grammar-neutral facts, generated behavior or selection movement, and real lowerer output for the canonical five BackendShape variants. No sixth shape, sidecar EventTape, public substrate API, or label-string lowerer can close.

### FNV Quarantine

V1 should say: W11L/W11N/W11O FNV closed-enum products are bench-only. They are not production equality, runtime selection, correctness proof, or generic hash arbiter. W10 must add production-root scans and adversarial semantic fixtures before any close claim.

## Pass Omega V9 Carry Forward

Omega-B recommends that subsequent V9 Omega agents consume this digest as follows:

| Consumer | Carry-forward obligation |
|---|---|
| Omega-A | Verify any V1 prose that still implies CSS 24/24 admission, generated CSS, Pattern H closure, or Decision Engine activation has current path-line evidence or is changed to diagnostic/open. |
| Omega-C | Convert repeated lessons into locks-diff proposals: anti-broadcast, gate-exclusion, generated provenance, aarch64 primitive gate, FNV quarantine, and Decision scaffold rejection. |
| Omega-D | Reconcile MASTER-PLAN waves around W0-W11 and the T-P1/T-P2/T-P3 governance notes. |
| Omega-E | Align skinny corpus pages so RESULTS/REDRESS/SK-V15 are classified consistently: JSON guard, CSS diagnostic, SK-V15 planning contract. |
| Omega-F | Handoff must stop at G-Omega before any CRUD or SK-V15 W0 implementation dispatch. |

This digest intentionally does not edit live surfaces. It records the skinny lessons that must be folded by the appropriate Omega/CRUD agents after challenge convergence and G-Omega authorization.
