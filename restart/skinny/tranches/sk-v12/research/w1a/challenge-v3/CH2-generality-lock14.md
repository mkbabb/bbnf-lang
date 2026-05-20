# SK-V12 W1a CHALLENGE V3 - CH2 Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14.
Disposition: ACCEPT.

## Finding

`PLAN-V3.md` closes the CH2 blockers from CHALLENGE V2. V2 required adding the
affected `passes/src/lib.rs` leak to ownership, aligning Rust/manual test
exclusion, and requiring deletion or scan coverage for retained
`sink_direct.rs` / `typed_direct.rs` stubs. V3 does all three.

Provider selection is now a data-driven boundary: `grammar_profile.rs` is
constrained to consumed grammar-neutral metadata, matches provider id to
`backend.grammar_name`, and keeps grammar-specific policy in provider-owned
modules. That is enough for W1a; it replaces the current generic choke point
where `codegen/src/lib.rs` hardwires `json_provider`, `sink_direct`, and
`typed_direct`.

The `passes/src/lib.rs` structural alphabet leak is correctly owned and
narrowly scoped. The current production leak is real, and V3 limits the edit to
deriving recognizer bytes from existing `GrammarIr` facts without directive,
BIR, `BackendShape`, public substrate API, or pass-wide policy expansion.

V3 preserves the no-expansion boundary. It explicitly excludes IR, runtime
tape/lib, grammar, SIMD, CSS/Sheets/BBNF-self roots, report/bin-gate/xtask
churn, and requires any outside path to return to plan. The W1a frozen-root
allowance is path-specific and must still reject directive, BIR, `BackendShape`,
public substrate, and unowned generic changes.

The scan semantics are executable enough for CH2. V3 requires the Lock 14 scan
to reject production JSON structural alphabet literals in generic roots, allows
JSON tokens in tests only under deliberate test-code exclusion, and requires the
manual sanity model to use the same exclusion semantics. It also requires
negative fixtures for all seven leak classes and positive fixtures for
JSON-owned roots.

The renderer-stub escape hatch is closed. Current `sink_direct.rs` is JSON
policy-bearing, and `typed_direct.rs` embeds JSON object/key delimiter policy.
V3 mandates deletion of the old generic-name files, with any retained
compatibility stub required to contain no JSON policy and be included in the
generic leak scan before acceptance.

## CH2 Disposition

ACCEPT. CH2 has no remaining plan-level blocker. Redress may proceed under V3,
but W1a still cannot claim CSS/SOTA/fallback admission or broaden public
substrate/API/IR/directive/`BackendShape` surfaces.
