# CH5 Hidden Coupling - SK-V13 Alpha V2

Date: 2026-05-21.

Verdict: REVISE.

The Alpha V2 packet fixes the V1 CH5 class for the union substrate: it makes
same-tape/codegen-private/row-consumed union routes binding, serializes
RESULTS/REDRESS ledgers, and blocks W0 behind G-Omega. One hidden coupling
remains: the current `bbnf-simd` classifier dispatch is grammar-parametric by
alphabet only, but the selected aarch64 classifier bakes JSON string semantics
(`"` quote, `\` escape, control threshold `0x20`) into the generic dispatch
path. E5 plans CSS and JSON SIMD consumers, so S-P3 must explicitly require a
per-grammar SIMD policy surface or a proven non-string-classification mode before
any CSS/union/parse_only row can consume `bbnf-simd` dispatch.

## Scope Checked

- SK-V13 Alpha packet: `SYNTHESIS.md`, `HANDOFF.md`, Alpha-C, Alpha-E, and V1
  hardening.
- Binding addendum: `USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`.
- Scoping docs: CSS parity, pass-framework/totality, value-api/union, and
  SIMD/ASM/union.
- Live code surfaces: `skinny/crates/codegen/src/lib.rs` and
  `skinny/crates/bbnf-simd/src/dispatch.rs`.
- Ledgers: current `skinny/RESULTS.md` and `skinny/REDRESS.md` authority
  references, with no edits.

## Findings

### F1 - Single Tape / No Sidecars

Disposition: ACCEPT.

Alpha V2 now carries the right substrate boundaries into the controlling
packet. `SYNTHESIS.md` limits union admission to parity, measured row movement,
same-wave consumer wiring, and no Track 1/Track 2 plane collapse
(`SYNTHESIS.md:73-82`). It also forbids SPEC-local authorization of a new
directive, BIR variant, `BackendShape`, public substrate API, or
grammar-specific generic behavior, and limits union to same-tape,
codegen-private, row-consumed variants (`SYNTHESIS.md:223-225`).

Alpha-E makes the operational gate concrete: E4 accepts only named row movement,
strict equality, no public substrate API, no retained sidecar class
column/vector/list/cursor, no `parse_only` demotion, and no JSON/CSS guard
demotion (`alpha-E-candidate-shortlist.md:324-329`). It also restates that no
parallel `UnionTape`, retained structural vector, or parser-owned cursor/list is
allowed (`alpha-E-candidate-shortlist.md:348-353`).

The value-api scoping doc still contains exploratory C1/C3 prose that can be
read as runtime routing or a retained vector index
(`sk-v13-scoping-value-api-union.md:318-337`, `:370-378`), but the Alpha V2
packet subordinates that prose: runtime union choice must be compile-time per
rule and legal union variants must share the single `Tape` and avoid sidecars
(`sk-v13-scoping-value-api-union.md:289-301`).

### F2 - JSON/CSS Shared Codegen

Disposition: ACCEPT.

The live codegen split is currently grammar-profile based, not an accidental
shared parser substrate. CSS is routed to
`css_l4_declaration_values_provider::emit_runtime_files()` while JSON requires
sink-only lowering plus JSON templates (`skinny/crates/codegen/src/lib.rs:153-181`).
That split is intentionally incomplete for full CSS parity, but Alpha V2 names
the coupling risk and constrains it:

- E2 exists specifically to move dispatch, string, number, flag, and sink shape
  out of JSON-specific generated paths into per-grammar generated modules
  consumed by CSS rows (`alpha-E-candidate-shortlist.md:135-196`).
- E2 rejects public traits, new directives, new BIR variants, new
  `BackendShape`s, public `UnionTape`-style substrate, or grammar-specific
  generic behavior (`alpha-E-candidate-shortlist.md:170-179`).
- S-P3 must refresh JSON guards whenever a wave changes generic runtime,
  codegen, generated output, benchmark, report, or gate paths that can produce
  JSON (`SYNTHESIS.md:108-110`).

That is enough for CH5: the shared-codegen path is risky, but the V2 contract
turns it into same-wave row evidence instead of hidden coupling.

### F3 - `bbnf-simd` Dispatch Policy Coupling

Disposition: REVISE.

The current dispatch selector accepts only an alphabet:
`select_classifier(alphabet)` stores the alphabet and chooses `Scalar` or
`NeonTbl4` (`skinny/crates/bbnf-simd/src/dispatch.rs:42-47`, `:89-98`). The
aarch64 TBL classifier then calls `classify_block_from_table` with JSON-specific
string parameters hardcoded in the generic dispatch arm:

- quote byte `b'"'`;
- escape byte `b'\\'`;
- control threshold `0x20`.

These are visible at `skinny/crates/bbnf-simd/src/dispatch.rs:23-32`.

That is harmless for current JSON-only string classification, but Alpha V2 E5
plans CSS delimiter/layout, CSS escaped identifiers, JSON structural,
JSON string, number, and `parse_only` consumers
(`alpha-E-candidate-shortlist.md:379-408`). A CSS or same-tape union consumer
that reaches this alphabet-only dispatch can silently inherit JSON quote,
escape, and control-byte semantics. That is a CH5 hidden coupling because the
call site can look grammar-neutral while the selected backend is not.

Required fix for S-P3:

1. Add an explicit SIMD classifier policy gate before any E5/C3 consumer:
   `bbnf-simd` dispatch must take a per-grammar classification policy
   (quote byte, escape byte, control threshold, and a mode for grammars where
   those concepts are not applicable), or the wave must prove the selected
   primitive never enters the string/control classifier path.
2. Add parity/checkasm cases for at least JSON policy, CSS identifier/string
   policy, and a delimiter-only/no-string policy. The gate must fail if a CSS
   row can reach the JSON hardcoded classifier constants.
3. Tie the policy to the same-wave row consumer. A policy-only dispatch API
   change remains support-only and cannot admit under E5.
4. Keep the policy codegen-private or `pub(crate)`; do not solve this by adding
   a public `GrammarConfig` trait or public substrate API.

This can be a small SPEC addition; it does not require changing Alpha's goalset.
It must be explicit because the live code surface is already coupled.

### F4 - RESULTS / REDRESS Ledgers

Disposition: ACCEPT.

Alpha V2 makes the ledgers single-writer surfaces. `HANDOFF.md` blocks
`skinny/RESULTS.md` and `skinny/REDRESS.md` edits before G-Omega
(`HANDOFF.md:54-91`) and requires redress phases that append either ledger to
serialize (`HANDOFF.md:93-106`). Alpha-E repeats that RESULTS and REDRESS are
single-writer ledgers even when redress worktrees run in parallel
(`alpha-E-candidate-shortlist.md:457-468`).

The telemetry binding also rejects stale run ids, mixed output planes,
permissive SOTA anchors, report-only Mbps, producer-only telemetry, missing
equality artifacts, and rows lacking provenance (`SYNTHESIS.md:153-185`). That
prevents a sidecar report from becoming admission evidence without ledger-gate
consumption.

### F5 - G-Omega Pre-W0 Block

Disposition: ACCEPT.

The pre-W0 block is now strong enough for CH5. `SYNTHESIS.md` says Totality V1.1
must be ratified before Wave 0, and no implementation Wave 0, source edit wave,
or RESULTS/REDRESS-writing wave may start until G-Omega closes
(`SYNTHESIS.md:112-122`). `HANDOFF.md` repeats the user-closed G-Omega
requirement before source, generated runtime, gate/report, or ledger edits
(`HANDOFF.md:54-74`), then lists pre-G-Omega allowed work as research/planning,
Omega/CHALLENGE, and read-only ledger inspection (`HANDOFF.md:76-91`).

This prevents hidden coupling between totality amendments and skinny
implementation waves by forcing the canonical surfaces to converge first.

### F6 - Totality Dependency

Disposition: ACCEPT.

The pass-framework scoping doc identifies the missing totality folds and routes
them to G-Omega: Lock 1 substrate-ceiling evidence, REDRESS-119/120 history,
Lock 14 per-wave gate language, Lock 16 SIMD/checkasm discipline, and non-JSON
telemetry schema (`sk-v13-scoping-pass-framework-leverage.md:186-258`). Alpha V2
then binds those same items into G6 (`SYNTHESIS.md:112-122`) and the Handoff
G-Omega fold surface (`HANDOFF.md:60-71`).

The dependency is visible rather than implicit: S-P1/S-P2/S-P3 may prepare
research and planning, but implementation and ledger writes remain blocked
until G-Omega and S-P3 convergence.

## Required V2 Fix

Add a CH5-specific S-P3/SPEC gate for `bbnf-simd` dispatch policy:

```text
G-SIMD-GRAMMAR-POLICY: any wave that wires `bbnf-simd` into CSS, union, JSON
parse_only, or shared generated code must prove the selected classifier uses
the consuming grammar's quote/escape/control policy or a no-string policy. The
gate must include scalar parity, checkasm/differential coverage for JSON and CSS
policies, same-wave row consumer measurement, no public substrate API, and no
retained sidecar classifier state. The current alphabet-only dispatch with
hardcoded JSON quote/escape/control constants is not admissible for non-JSON
consumers.
```

## Final Disposition

REVISE. The packet is otherwise CH5-sound: single tape/no sidecars, JSON/CSS
codegen row gates, ledgers, G-Omega, and totality dependency are all explicit.
The remaining blocker is narrow but real: `bbnf-simd` dispatch must not let
non-JSON consumers inherit JSON string semantics through an alphabet-only
backend selector.
