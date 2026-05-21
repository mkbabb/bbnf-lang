---
lens: CH2
name: GENERALITY / LOCK 14
pass: T-P3-synthesis
cycle: V1
generated_at: 2026-05-21T20:13:00-04:00
disposition: ACCEPT
scope: "CH2 generality and Lock 14 only"
artifacts_audited:
  - restart/prompts/totality/PASS-3-SYNTHESIS.md
  - restart/audit/totality/p3/3A-architecture-synthesis.md
  - restart/audit/totality/p3/3B-master-plan-reconciliation.md
  - restart/audit/totality/p3/3C-locks-crystallisation.md
  - restart/audit/totality/p3/3C-locks-v+1-diff.md
  - restart/audit/totality/p3/3D-skinny-fold.md
  - restart/audit/totality/p3/3E-grammar-generalisation.md
  - restart/audit/totality/p3/3F-migration-handoff.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md
  - restart/audit/totality/p2/2A-sota-landscape.md
  - restart/audit/totality/p2/2B-primitive-vocabulary.md
  - restart/audit/totality/p2/2C-grammar-neutrality.md
  - restart/audit/totality/p2/2D-cost-model.md
  - restart/audit/totality/p2/2E-host-arch-esoterica.md
  - restart/audit/totality/p2/2F-parse-that-gaps.md
  - restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md
  - restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md
  - restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md
  - restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md
---

# T-P3 V1 CH2 Generality / Lock 14

## Lens Contract

PASS-3 defines CH2 as the Lock 14 generality check: 3A and 3B must generalize
to non-JSON, 3E must be concrete for CSS L4 / Sheets / BBNF-self, 3C must
accept no JSON-narrowing lock amendment, and the future-grammar onboarding test
must survive (`restart/prompts/totality/PASS-3-SYNTHESIS.md:108`-`111`).
The bbnf-specific axes make Lock 14 the binding generalisation discipline,
require generic crates to remain grammar-neutral, and assign the onboarding
test to 3E (`restart/prompts/totality/PASS-3-SYNTHESIS.md:210`-`214`).

## Verdict

ACCEPT.

T-P3 V1 satisfies CH2. The packet does not paper over Lock 14 with a JSON-only
story: 3A, 3B, 3C, and 3E all preserve generated grammar ownership, require
name and shape leak scans, keep CSS L4 as admitted evidence rather than
universal closure, and force Sheets / BBNF-self negative controls before
fleet-wide transfer. The remaining manifest-layout and fact-stream-placement
questions are routed as downstream schema choices, not as Lock 14 weakening.

## Evidence

| check | disposition | evidence |
|---|---|---|
| Governing T-P1/T-P2 state | ACCEPT | T-P1 converged with CH2 accepted while preserving the load-bearing caveats: Lock 14 remains drifted where generic crates carry grammar-name or grammar-shape policy, generated per-grammar names are allowed only behind generated/rostered criteria, and CSS L4 declaration-values is evidence but not full CSS parity or universal closure (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:21`-`22`, `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:45`-`51`). T-P2 then accepted CH2 with generated-policy/caller-data ownership, CSS/Sheets pressure, and no JSON-only generic-crate policy (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md:21`-`28`). |
| T-P2 Lock 14 transfer contract | ACCEPT | The inherited contract requires a generated provider registry, grammar-shape leak scan, generated sink/fact/value/flag surfaces, primitive policy manifest, CSS plus Sheets or BBNF-self negative-control transfer, and decision-engine facts (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`64`). The per-technique table explicitly covers CSS L4, Sheets, and BBNF-self for byte-set classify/run-skip, string/escape scan, digit/number scan, direct/fact sink, BackendShape resolver, and regex/HIR facts (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:66`-`75`). |
| 3A architecture generalizes beyond JSON | ACCEPT | 3A preserves the five `BackendShape` canon while replacing the hardcoded derive cascade with generated candidate/constraint/cost evidence (`restart/audit/totality/p3/3A-architecture-synthesis.md:35`). It adds CSS fact-stream taxonomy as admitted non-JSON evidence, not retained substrate or full CSS closure (`restart/audit/totality/p3/3A-architecture-synthesis.md:38`, `restart/audit/totality/p3/3A-architecture-synthesis.md:53`). It also replaces hardcoded provider/profile status with a generated registry/manifest contract that forbids generic grammar branches and inferred JSON roles, with CSS/Sheets/BBNF-self negative controls (`restart/audit/totality/p3/3A-architecture-synthesis.md:39`, `restart/audit/totality/p3/3A-architecture-synthesis.md:54`). |
| 3B planning keeps Lock 14 concrete | ACCEPT | 3B adds MP.NW6 for generated registry, grammar-owned surfaces, grammar-name plus grammar-shape scans, and CSS plus Sheets/BBNF-self negative controls (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:117`). It adds MP.NW8 with JSON/CSS/Sheets/BBNF-self backend-shape rows and bounded resolver reports (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:119`). It adds MP.NW11 for Sheets and BBNF-self generalization witnesses or explicit fail-closed telemetry (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:122`). The open cardinality question is explicitly routed to G-Omega/user sign-off, not silently weakened (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:153`). |
| 3E matrix is concrete for CSS L4 / Sheets / BBNF-self | ACCEPT | 3E states the non-JSON story mechanically: keep five `BackendShape` variants while generating every selection, primitive policy, sink, flag, and provider surface from grammar source plus workspace metadata (`restart/audit/totality/p3/3E-grammar-generalisation.md:23`-`38`). Its BackendShape matrix covers CSS stylesheet/selector/declaration/value rules, Sheets formulas/functions/arrays/infix expressions, and BBNF-self grammar/expression/directive routes without adding a sixth shape (`restart/audit/totality/p3/3E-grammar-generalisation.md:61`-`75`). Its primitive transfer table maps all three grammars across byte classes, strings, numbers, direct/fact sinks, regex/HIR facts, resolver facts, and SIMD/ASM gates (`restart/audit/totality/p3/3E-grammar-generalisation.md:77`-`87`). |
| Future-grammar onboarding test is concrete | ACCEPT | 3E requires the future grammar to add only grammar source, workspace metadata, and allowed host functions; regenerate provider/config/fact/sink/value/view/path/diagnostic/test surfaces; keep generic-crate diffs empty except generated runtime output; run Lock 14 name and shape leak scans; emit a five-shape eligibility report; attach primitive-policy manifests; pair a positive row with Sheets or BBNF-self negative-control evidence when claiming generality; and fail closed if onboarding needs a new directive, BIR variant, BackendShape, public substrate API, retained sidecar, or hand-coded generic behavior (`restart/audit/totality/p3/3E-grammar-generalisation.md:89`-`117`). This matches the T-P2 onboarding test and mandatory CSS plus negative-control rule (`restart/audit/totality/p2/2C-grammar-neutrality.md:157`-`168`). |
| 3C accepts no JSON-narrowing Lock 14 amendment | ACCEPT | 3C routes all 41 candidates with 30 ACCEPT, 11 MODIFY, 0 REJECT, and 0 DEFER, avoiding silent drops (`restart/audit/totality/p3/3C-locks-crystallisation.md:42`-`50`). Every Lock 14 disposition strengthens generality: generated non-JSON allowance with name/shape criteria, generated provider manifest, grammar-shape leak scan, grammar-owned sink/fact/flag semantics, onboarding proof over five shapes, non-JSON exercise for claimed grammar-neutral primitives, generated grammar metadata, and grammar-neutral parse-that/regex APIs (`restart/audit/totality/p3/3C-locks-crystallisation.md:63`-`66`, `restart/audit/totality/p3/3C-locks-crystallisation.md:70`-`83`, `restart/audit/totality/p3/3C-locks-crystallisation.md:86`, `restart/audit/totality/p3/3C-locks-crystallisation.md:93`). None narrows a lock to JSON. |
| Generated/provider exceptions are bounded | ACCEPT | The 3C proposed Lock 14 diff permits generated files under `runtime/src/grammars/<name>/` to contain grammar names only when emitted by the rostered generator from grammar source plus metadata, and explicitly excludes hand-coded provider enums, root aliases, generic-crate grammar branches, grammar-named generic public APIs, generic-root proof fixtures, and grammar-shaped policy mining (`restart/audit/totality/p3/3C-locks-v+1-diff.md:270`-`277`). Generic crates may consume generated manifests and generated surfaces but may not hand-code `RuntimeProvider::{Json, CssL4DeclarationValues}`, JSON/CSS renderer branches, JSON alphabets, JSON role mining, sink callback names, or grammar-specific feature flags (`restart/audit/totality/p3/3C-locks-v+1-diff.md:279`-`286`). Per-wave gates require Lock 14 baseline plus name/shape census for generic crates, manifests, runtime roots, codegen templates, decision facts, and shared primitive consumers (`restart/audit/totality/p3/3C-locks-v+1-diff.md:288`-`309`). |
| CSS row remains evidence, not universal closure | ACCEPT | 3C's diff supersedes the old scoped allowance with SK-V12 CSS declaration-values as a same-plane fact-stream row, while stating it is not full CSS parity, universal grammar closure, or a generic-crate exception (`restart/audit/totality/p3/3C-locks-v+1-diff.md:39`-`48`). Lock 8 row-plane wording repeats that CSS L4 declaration-values is a `PASS-ADMIT` row, not full CSS parity, universal grammar closure, or SK-V13 close authority (`restart/audit/totality/p3/3C-locks-v+1-diff.md:140`-`147`). 3E likewise makes CSS the positive proof lane while preserving Sheets and BBNF-self as JSON-role-mining falsifiers (`restart/audit/totality/p3/3E-grammar-generalisation.md:23`-`37`). |
| Fact streams and provider layout open questions do not block CH2 | ACCEPT | 3E's own CH2 open question routes CSS fact streams to 3A + 3C with the gate that accepted wording must preserve five shapes and the T-P2 output-plane rule (`restart/audit/totality/p3/3E-grammar-generalisation.md:160`-`164`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:96`-`98`). The provider-manifest layout remains an implementation/API-minimization question, but 2C says closure is not optional and a hand-coded provider enum remains Lock 14 drift (`restart/audit/totality/p2/2C-grammar-neutrality.md:170`-`176`). |

## Repairs

Required blocking repairs: none.

Carry-forward constraints for Pass Omega / S-P3:

1. Preserve the exact Lock 14 fence from 3C: generated grammar names are allowed
   only as rostered generated output, never as hand-coded generic provider or
   role-policy branches.
2. Do not reduce the negative-control rule below the T-P2/3E standard. A
   fleet-wide generality claim needs CSS L4 plus Sheets or BBNF-self
   witness/negative-control; the single CSS declaration-values row remains
   admitted evidence only.
3. Resolve the provider-manifest layout in the Lock 14 registry wave by proving
   JSON, CSS, and a Sheets or BBNF-self provider without editing generic code.
4. Resolve CSS fact-stream placement as an output-plane taxonomy or `SinkOnly`
   product only if the five-shape canon and no-retained-sidecar rule are
   preserved.
5. Keep shared primitive policy caller/generated-owned. JSON punctuation,
   string, number, quote, escape, and no-string/no-number policy must not become
   shared crate constants.

## Cycle Disposition

CH2 disposition for T-P3 V1: ACCEPT.
