# T-P2 V1 CH5 Hidden Coupling / Lock 1

Pass: T-P2 Research. Cycle: V1. Lens: CH5 hidden coupling / Lock 1.
Date: 2026-05-21.
Scope: `restart/audit/totality/p2/2A-sota-landscape.md` through
`2F-parse-that-gaps.md`.

## Verdict

REVISE.

V1 is directionally sound: the dossiers repeatedly reject retained
class-column replay, sidecar producers, proof-only primitives, and
grammar-specific policy in shared layers. The pass is not paper-closing the old
W3 route. However, CH5 cannot accept V1 yet because several grounded designs
still leave implicit substrate boundaries rather than enforceable ones. The
fold needs to make the single-substrate invariant mechanical for the
decision-engine, collapsed-stage, parse-that import, and primitive-layer
interfaces.

This is not a rejection of the research. It is a required V2 tightening before
T-P3 can turn the research into locks amendments.

## Evidence

### What V1 Gets Right

| area | evidence | CH5 read |
|---|---|---|
| SOTA structural-index transfer | 2A states simdjson stage 1 transfers only as a transient producer consumed into the existing tape/sink, and says retained class-column replay is refuted by REDRESS 96/97/98. | Accept. This directly protects Lock 1. |
| Two-layer primitive vocabulary | 2B keeps Layer 0 as vendored macro/process infrastructure and Layer 1 as bbnf-authored byte/mask/carry primitives with grammar policy supplied by caller/generated data. | Accept with one fold. The layer split is clean, but V2 must forbid Layer 1 from storing policy-derived retained state. |
| Grammar-neutral policy surface | 2C refutes `JsonSink`, hardcoded `RuntimeProvider`, and generic JSON role mining as totality surfaces; it moves sink/fact/flag meanings into generated grammar-owned surfaces. | Accept. This is the right Lock 14/Lock 1 separation. |
| Historical union route | 2D and 2F cite REDRESS 96/97/98 and require material differentials before any new union attempt. | Accept with tighter wording. The category is open under the user pin, but replay of retained vector/cursor sidecars is correctly blocked. |
| SIMD inventory | 2B and 2E distinguish microbench/checkasm prerequisites from production admission and require same-wave consumers. | Accept. This prevents support primitives from becoming hidden producers. |

### Blocking Coupling Risks

| id | dossier | coupling risk | why it blocks CH5 |
|---|---|---|---|
| CH5-R1 | 2D cost model / e-graph | The e-graph/CSP resolver is allowed to enumerate `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage` plans, but V1 does not require each `BackendExpr` node or rewrite to declare whether it introduces retained state, transient masks, existing tape columns, or direct sink facts. | A resolver can otherwise smuggle a parallel substrate in as an optimization artifact rather than a source-level `UnionTape`. V2 must add a substrate-kind annotation and extraction-time rejection rule. |
| CH5-R2 | 2D `CollapsedStage` | V1 correctly calls `CollapsedStage` a hardware-gated transient FSM producer, but does not define the lifetime boundary of its mask/FSM streams. | CH5 needs a rule that collapsed-stage masks/classes are local temporaries consumed within the emitted kernel or row sink, never persisted into a tape, public API, parser state, or fact stream unless that fact stream is the admitted output plane. |
| CH5-R3 | 2F parse-that import | 2F recommends importing upstream parse-that regex/HIR/scanner machinery as `bbnf-regex`, but the fold does not yet distinguish compile-time scanner facts from runtime scanner streams/caches. | Importing scanner-plan machinery could create a second substrate if runtime span scanners retain masks, class streams, or cursor state across parser phases. V2 must state: import facts/plans/HIR; runtime masks are transient and consumed by generated grammar code in the same loop. |
| CH5-R4 | 2C generated sink and flag surface | 2C proposes grammar-owned sink/fact/value/flag surfaces, but it does not define the boundary between admitted fact-stream output and hidden substrate sidecars. | CSS fact streams are valid row outputs, but a "fact surface" cannot become a generic retained substrate that later waves consume out-of-band. V2 must require every fact stream to be an output-plane contract with comparator/oracle/gate provenance, not an internal sidecar. |
| CH5-R5 | 2B / 2E primitive manifest | Both dossiers require same-wave consumers, but the manifest fields do not yet include `retention_lifetime` or `substrate_target`. | Without these fields, a primitive can be wired to a consumer while still leaving retained masks/classes in parser state. V2 must add manifest fields that prove the primitive consumes into `existing_tape`, `direct_sink`, `fact_output`, or `local_temp_only`; any other value is REVISE. |

## Blockers And Fold Requirements

1. Add a Lock 1 substrate-kind rule to the V2 dossiers: every e-graph
   candidate, backend rewrite, imported scanner plan, union candidate, and SIMD
   primitive consumer must declare one of `local_temp_only`, `existing_tape`,
   `direct_sink`, or `admitted_fact_output`. Anything that requires a retained
   class/mask stream, parser-owned sidecar, public substrate API, or second
   tape is a CH5 REJECT unless the user explicitly amends Lock 1.

2. Extend the Lock 16 primitive manifest proposed by 2A/2B/2E with
   `retention_lifetime`, `substrate_target`, and `policy_owner` fields.
   `policy_owner` must be `generated_grammar` or `caller_data` for shared
   primitives; shared runtime/primitive crates cannot own grammar policy.

3. Tighten 2D's `CollapsedStage` language: it is an emitted-kernel strategy, not
   a retained FSM substrate. Any mask/class/FSM state is ephemeral and consumed
   inside the generated function or into an admitted row output. AVX-512
   literature remains totality background; it is not proof of a retained
   aarch64 sidecar.

4. Tighten 2F's parse-that route: `bbnf-regex` may import HIR, byte classes,
   regex facts, automata facts, and compile-time scanner plans. It must not
   import or expose a runtime scanner substrate that persists masks/classes or
   cursor state across generated parser phases. Generated grammar code owns the
   consumer loop.

5. Tighten 2C's fact-stream rule: a CSS or future grammar fact stream is valid
   only as an output plane with strict comparator/oracle provenance and
   gate-consumed telemetry. It is not a generic substrate for later internal
   consumers unless separately admitted under Lock 1.

6. Preserve the accepted fences from V1: no new directive, no new BIR variant,
   no new `BackendShape`, no `UnionTape`, no parser-owned sidecar slots, no
   public substrate API, and no replay of REDRESS 96/97/98 class-column or
   streaming-cursor routes without a material differential and CHALLENGE.

## Disposition By Dossier

| dossier | disposition | required V2 fold |
|---|---|---|
| 2A SOTA landscape | ACCEPT-WITH-FOLD | Carry `T2A-LAC-01` forward and add the substrate-kind vocabulary so transient stage-1-style masks cannot become retained sidecars. |
| 2B primitive vocabulary | REVISE | Add manifest fields for retention lifetime, substrate target, and policy owner; explicitly forbid Layer 1 from retaining grammar-policy-derived masks/classes. |
| 2C grammar neutrality | REVISE | Distinguish admitted fact-stream output planes from hidden sidecars; add comparator/oracle/gate provenance as the fact-stream boundary. |
| 2D cost model | REVISE | Require e-graph nodes, rewrite guards, and extraction results to carry substrate-kind annotations; define `CollapsedStage` masks as local temporaries only. |
| 2E host-arch esoterica | ACCEPT-WITH-FOLD | Hardware gates are clean; add the same retention/substrate manifest fields to the Lock 16 hardware-gate manifest. |
| 2F parse-that gaps | REVISE | Split imported compile-time regex/scanner facts from runtime scanner streams; generated grammar code must consume scanner outputs in-loop. |

## Cycle Disposition

CH5 disposition for T-P2 V1: REVISE.

The V2 acceptance target is narrow and testable: the totality research may keep
the same candidate architecture, but every candidate path must expose where its
masks/classes/facts live, who owns policy, and whether the data is transient,
existing-substrate, direct-output, or admitted fact-output. If that is folded,
CH5 should be able to accept without demanding a new research direction.
