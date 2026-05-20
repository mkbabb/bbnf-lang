# SK-V12 W1a CHALLENGE V2 - CH2 Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14.
Disposition: REVISE.

## Finding

`PLAN-V2.md` fixes the V1 provider-boundary shape in principle: provider
selection is specified as data-driven lookup, JSON literals stay in
`json_provider` / JSON-owned roots, and W1a still forbids public substrate/API,
IR, BIR, `BackendShape`, and directive expansion.

CH2 cannot ACCEPT yet because the executable Lock 14 proof cannot pass with the
stated owner roster. `PLAN-V2.md` requires the scan to include
`crates/passes/src`, but that generic root currently contains the exact JSON
structural alphabet literal the plan says the scan must reject:
`skinny/crates/passes/src/lib.rs:340`. `PLAN-V2.md` also excludes
`skinny/crates/passes/src/` from W1a ownership, so redress would either fail its
own scan or weaken the proof.

## Required Revision

1. Either add the affected generic passes root to the W1a owner roster and
   remove/derive the JSON structural alphabet there, or explicitly revise the
   scan policy with a CHALLENGE-accepted reason that this production literal is
   not a Lock 14 leak.
2. Make the Rust scan and manual sanity command agree on test exclusion. The
   raw `rg` check currently hits inline `#[cfg(test)]` JSON tokens in generic
   roots.
3. Require deletion or scan coverage for any retained `sink_direct.rs` /
   `typed_direct.rs` compatibility stubs; otherwise JSON renderer leaks can
   survive outside the declared JSON-owned roots.

## CH2 Disposition

REVISE. The provider boundary can pass after the generic passes-root leak and
stub-scan policy are made executable.
