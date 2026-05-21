# CH5 Hidden Coupling - SK-V13 Alpha V4

Date: 2026-05-21.

Verdict: ACCEPT.

V4 rechecked the V3 consolidated result against the current Alpha packet and
the live code surfaces. The CH5 disposition remains ACCEPT. The packet's
`G-SIMD-GRAMMAR-POLICY` covers the `bbnf-simd` alphabet-only classifier hazard
for CSS, union, JSON `parse_only`, and shared generated-code consumers, and the
surrounding gates continue to close the single-tape, codegen, ledger,
G-Omega, and totality coupling surfaces.

## Scope Checked

- V3 consolidated: `restart/skinny/tranches/sk-v13/research/alpha-hardening/V3/CONSOLIDATED.md`.
- V3 CH5: `restart/skinny/tranches/sk-v13/research/alpha-hardening/V3/CH5-hidden-coupling.md`.
- Current packet: `restart/skinny/tranches/sk-v13/SYNTHESIS.md`,
  `restart/skinny/tranches/sk-v13/HANDOFF.md`, Alpha-E, and Alpha-F.
- Live implementation references:
  `skinny/crates/bbnf-simd/src/dispatch.rs` and
  `skinny/crates/codegen/src/lib.rs`.

## Findings

### F1 - SIMD Grammar Policy Covers Alphabet-Only JSON Constants

Disposition: ACCEPT.

The underlying hazard is still real in live code. `SelectedClassifier` carries
only an alphabet plus backend, while the aarch64 `NeonTbl4` path passes
hardcoded JSON string constants into `classify_block_from_table`: quote
`b'"'`, escape `b'\\'`, and control threshold `0x20`
(`skinny/crates/bbnf-simd/src/dispatch.rs:5-32`). Backend selection remains
alphabet-only through `select_backend(alphabet)` and `lo6_table_admissible`
(`skinny/crates/bbnf-simd/src/dispatch.rs:42-47`, `:89-112`).

The current packet covers that exact coupling. `SYNTHESIS.md` requires any SPEC
wave wiring `bbnf-simd` into CSS, union, JSON `parse_only`, or shared generated
code to include `G-SIMD-GRAMMAR-POLICY`. The selected classifier must use the
consuming grammar's quote/escape/control policy or a no-string policy, with
scalar parity, checkasm/differential coverage for JSON and CSS policies,
same-wave measured row consumption, no public substrate API, and no retained
sidecar classifier state (`SYNTHESIS.md`, Section 4). Section 5 then pre-blocks
non-JSON or shared consumers of alphabet-only classifier dispatch unless that
policy proves they cannot inherit JSON quote/escape/control constants.

Alpha-E repeats the live risk and binds the same falsifiability rule to E5. It
names the JSON constants in `dispatch.rs`, requires policy coverage for JSON,
CSS identifier/string, and delimiter-only/no-string cases, and states that the
current alphabet-only dispatch with hardcoded JSON constants is not admissible
for non-JSON consumers. `HANDOFF.md` carries the same condition into downstream
refusal criteria for CSS, union, JSON `parse_only`, and shared generated code.

This is sufficient for CH5: the packet does not claim the implementation is
already grammar-safe. It correctly treats the current implementation as unsafe
for non-JSON/shared reuse until a named gate proves the consuming grammar policy
and consumes it in a measured row.

### F2 - Single Tape / No Sidecars

Disposition: ACCEPT.

The single-tape boundary remains fail-closed. `SYNTHESIS.md` unblocks union
only for same-tape, codegen-private, row-consumed variants and forbids new
public substrate APIs or grammar-specific generic behavior. Alpha-E's union
gate requires strict equality, no public substrate API, no retained sidecar
class column/vector/list/cursor, no `parse_only` demotion, and no silent
JSON/CSS guard demotion.

The C3 SIMD-first union route is therefore not a sidecar escape hatch. It can
only land if its lane-index policy inherits E5 checks, wires into a JSON
structural or `parse_only` measured row in the same wave, and preserves the
single shared tape surface.

### F3 - Codegen Coupling

Disposition: ACCEPT.

The current codegen split still routes CSS declaration-values through
`css_l4_declaration_values_provider::emit_runtime_files()` and JSON through
sink-only lowering plus JSON templates (`skinny/crates/codegen/src/lib.rs:153-181`).
That split is not treated as a complete solution. The packet requires
grammar-neutral policy data to be private to codegen/runtime generation, rejects
public `GrammarConfig` surfaces, and requires every shared generated-code or
SIMD policy change to be consumed by a same-wave measured CSS/JSON row.

That closes the hidden coupling risk: shared generated code cannot silently
reuse JSON-specific classifier behavior, and generic config fields cannot land
as unmeasured support inventory.

### F4 - Ledgers

Disposition: ACCEPT.

Ledger coupling remains controlled. `SYNTHESIS.md` binds JSON, CSS, union,
SIMD, and decision-engine rows to a common telemetry schema and rejects stale
run ids, mixed output planes, permissive SOTA anchors, report-only Mbps,
producer-only telemetry, missing equality artifacts, and rows without
provenance. `HANDOFF.md` blocks RESULTS/REDRESS-writing waves before G-Omega
and requires redress phases that append either ledger to serialize. Alpha-E
also states that RESULTS and REDRESS are single-writer ledgers even when
parallel redress worktrees run.

### F5 - G-Omega

Disposition: ACCEPT.

G-Omega remains a hard pre-W0 dependency. `SYNTHESIS.md` requires Totality V1.1
ratification before Wave 0 and blocks implementation Wave 0, source edit waves,
and RESULTS/REDRESS-writing waves until G-Omega closes. `HANDOFF.md` repeats
that closure requirement and limits pre-G-Omega work to research, planning,
Omega/CHALLENGE work, and read-only ledger inspection.

### F6 - Totality Dependency

Disposition: ACCEPT.

The totality dependency is still visible rather than implicit. The packet says
G-Omega must fold the SK-V12 CSS admission, GrammarConfig/Lock 14 evidence,
REDRESS-119/120/121-127 lessons, Lock 16 SIMD/checkasm discipline, non-JSON
telemetry schema, and zero-orphan evidence into canonical totality surfaces
before implementation begins. That ordering prevents CH5 policy and SIMD/union
exceptions from bypassing the totality gate.

## Final Disposition

ACCEPT. V4 confirms V3 CH5. `G-SIMD-GRAMMAR-POLICY` covers the `bbnf-simd`
alphabet-only JSON-constant hazard for CSS, union, JSON `parse_only`, and shared
generated consumers. The packet also preserves the required adjacent coupling
guards: single tape/no sidecars, same-wave codegen consumption, serialized
ledgers, pre-W0 G-Omega closure, and Totality V1.1 dependency.
