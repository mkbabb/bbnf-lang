# CH5 Hidden Coupling - SK-V13 Alpha V3

Date: 2026-05-21.

Verdict: ACCEPT.

The V3 Alpha packet fully handles the V2 hidden-coupling blocker. The new
`G-SIMD-GRAMMAR-POLICY` is now binding in the controlling packet, repeated in
the E5 candidate gate, and surfaced as a downstream Handoff reject condition.
It directly covers the live `bbnf-simd` risk: the current dispatch selector is
alphabet-only while the aarch64 TBL path bakes JSON quote, escape, and control
constants into the selected classifier.

## Scope Checked

- V2 CH5: `restart/skinny/tranches/sk-v13/research/alpha-hardening/V2/CH5-hidden-coupling.md`.
- Current Alpha packet: `SYNTHESIS.md`, `HANDOFF.md`, and Alpha-E.
- Current Alpha hardening V2 consolidated required fold.
- Scoping docs for union, SIMD/ASM, pass-framework/totality, CSS parity, and
  profile truth.
- Live code surfaces: `skinny/crates/bbnf-simd/src/dispatch.rs` and
  `skinny/crates/codegen/src/lib.rs`.

## Findings

### F1 - `G-SIMD-GRAMMAR-POLICY` vs Alphabet-Only Dispatch

Disposition: ACCEPT.

The V2 blocker was narrow: `SelectedClassifier` stores only an alphabet and
selects the backend from that alphabet (`skinny/crates/bbnf-simd/src/dispatch.rs:5-9`,
`:42-47`, `:89-98`), but the aarch64 `NeonTbl4` arm calls
`classify_block_from_table` with JSON-specific hardcoded constants:

- quote byte `b'"'`;
- escape byte `b'\\'`;
- control threshold `0x20`.

Those constants remain visible in the live code
(`skinny/crates/bbnf-simd/src/dispatch.rs:23-32`). That is acceptable only
because V3 no longer permits non-JSON/shared consumers to reach that path
without a grammar policy proof.

`SYNTHESIS.md` now requires any SPEC wave wiring `bbnf-simd` into CSS, union,
JSON `parse_only`, or shared generated code to include
`G-SIMD-GRAMMAR-POLICY`; the selected classifier must use the consuming
grammar's quote/escape/control policy or a no-string policy, with scalar
parity, checkasm/differential coverage for JSON and CSS policies, same-wave
measured row consumption, no public substrate API, and no retained sidecar
classifier state (`SYNTHESIS.md:226-231`). The explicit reject list then closes
the exact loophole: non-JSON or shared consumers of alphabet-only classifier
dispatch reject unless the policy proves they cannot inherit the JSON constants
(`SYNTHESIS.md:248-250`).

Alpha-E makes the same gate more concrete for E5. It records the live
alphabet-only dispatch and JSON constants (`alpha-E-candidate-shortlist.md:394-398`),
names CSS delimiter/string/number and JSON structural/string/number/`parse_only`
consumers (`:404-413`), and requires differential cases for JSON policy, CSS
identifier/string policy, and delimiter-only/no-string policy. It also states
that the current alphabet-only dispatch with hardcoded JSON constants is not
admissible for non-JSON consumers (`:420-429`).

`HANDOFF.md` repeats this as a downstream REVISE trigger for CSS, union, JSON
`parse_only`, or shared generated code without the policy proof, scalar parity,
checkasm/differential coverage, same-wave row measurement, no public substrate
API, and no sidecar classifier state (`HANDOFF.md:155-159`). This is sufficient
for CH5: the hidden coupling is no longer implicit or papered over; it is a
named fail-closed gate.

### F2 - Single Tape / No Sidecars

Disposition: ACCEPT.

The single-tape constraint remains explicit. `SYNTHESIS.md` unblocks union only
for same-tape, codegen-private, row-consumed variants and forbids public
substrate APIs or grammar-specific generic behavior (`SYNTHESIS.md:223-225`).
Alpha-E's union gate requires strict equality, no public substrate API, no
retained sidecar class column/vector/list/cursor, no `parse_only` demotion, and
no silent JSON/CSS guard demotion (`alpha-E-candidate-shortlist.md:326-329`).

The scoping doc still contains exploratory C3 vector-lane prose, but the
controlling packet subordinates it. The legal union substrate must share the
single `Tape<'input>`, avoid parser-owned runtime union routing, and avoid
sidecar vectors (`sk-v13-scoping-value-api-union.md:289-292`). V3 keeps that
boundary intact.

### F3 - JSON/CSS Codegen Coupling

Disposition: ACCEPT.

The live codegen split remains grammar-profile routed: CSS declaration-values
uses `css_l4_declaration_values_provider::emit_runtime_files()`, while JSON
requires sink-only lowering and JSON templates (`skinny/crates/codegen/src/lib.rs:153-181`).
V3 does not pretend that split is enough for full CSS; instead, it turns shared
codegen changes into row-bound obligations.

E2 is specifically scoped to remove JSON policy leaks from generated runtime
behavior without adding a public `GrammarConfig` trait or substrate API
(`alpha-E-candidate-shortlist.md:137-139`). Each policy expansion must be
consumed by a generated grammar row in the same wave, and a generic config field
with no CSS/generated consumer rejects (`:163-168`). The gate also requires
strict CSS parity, preserved JSON guards, and no generic branch on grammar name
or JSON-specific roles (`:172-178`). This is enough to prevent shared codegen
from becoming an unmeasured coupling surface.

### F4 - Ledgers

Disposition: ACCEPT.

The ledger coupling remains closed. `SYNTHESIS.md` requires complete telemetry
fields for JSON, CSS, union, SIMD, and decision-engine reports and rejects stale
run ids, mixed output planes, permissive SOTA anchors, report-only Mbps,
producer-only telemetry, missing equality artifacts, and rows lacking
provenance (`SYNTHESIS.md:155-185`). `HANDOFF.md` blocks pre-G-Omega edits to
`skinny/RESULTS.md` and `skinny/REDRESS.md` (`HANDOFF.md:85-91`) and requires
redress phases that append either ledger to serialize (`:105-106`). Alpha-E
repeats that RESULTS and REDRESS are single-writer ledgers even with parallel
redress worktrees (`alpha-E-candidate-shortlist.md:482-483`).

### F5 - G-Omega

Disposition: ACCEPT.

G-Omega remains a hard pre-W0 gate. `SYNTHESIS.md` requires Totality V1.1 before
Wave 0 and blocks implementation Wave 0, source edit waves, and RESULTS/REDRESS
writing waves until G-Omega closes (`SYNTHESIS.md:112-122`). `HANDOFF.md`
requires user closure before source, generated runtime, gate/report, or ledger
edits (`HANDOFF.md:56-58`) and limits pre-G-Omega work to research/planning,
Omega/CHALLENGE work, and read-only ledger inspection (`:78-91`).

### F6 - Totality Dependency

Disposition: ACCEPT.

The totality dependency is visible and ordered. The pass-framework scoping doc
identifies the missing folds for union substrate history, REDRESS-119/120,
Lock 14 per-wave gate language, Lock 16 SIMD/checkasm discipline, and non-JSON
telemetry schema (`sk-v13-scoping-pass-framework-leverage.md:186-258`).
`SYNTHESIS.md` and `HANDOFF.md` bind those lessons into the G-Omega surface
before implementation waves can start (`SYNTHESIS.md:114-122`,
`HANDOFF.md:60-71`).

## Final Disposition

ACCEPT. V3 resolves the V2 CH5 blocker. `G-SIMD-GRAMMAR-POLICY` fully handles
the `bbnf-simd` alphabet-only dispatch hazard for CSS, union, JSON `parse_only`,
and shared generated consumers, including the hardcoded JSON quote, escape, and
control constants. The other CH5 surfaces remain closed: single tape/no
sidecars, JSON/CSS codegen coupling, ledgers, G-Omega, and totality dependency
are explicit fail-closed obligations.
