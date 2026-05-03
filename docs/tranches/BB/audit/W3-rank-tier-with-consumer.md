# W3 Rank/Tier Rewrites With Same-Wave Consumer

Date: 2026-05-03
Scope: The Era V abrogation evidence per `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:8-12` + `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:107-127`. Records the atomic-commit discipline for rank.rs + tiering.rs creation alongside their consumer.

## §1 The Era V failure mode

The Era V DTA-Ψ-RUT archaeology (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:7-10`) identified the substrate-first / consumer-later anti-pattern: a wave produces substrate (rank.rs, tiering.rs) without a same-wave or next-wave consumer, leaving the substrate as zero-caller dead code that requires a follow-up wave to wire up. The follow-up wave then either (a) discovers the substrate is misshaped and the work is wasted, or (b) ratifies the substrate against an unwritten contract and propagates the misshapen contract forward.

The synthesis pass at `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:118-127` ratifies the corrective amendment:

> Add File Bounds:
> | `crates/ir/src/rewrites/rank.rs` | create |
> | `crates/ir/src/rewrites/tiering.rs` | create |
>
> Hard Gate 1 becomes:
> 1. `crates/ir/src/rewrites/rank.rs` and `tiering.rs` are created, implemented, and consumed by the W3 run in the same wave.

## §2 BB.W3c atomic commit structure

The BB.W3c milestone discipline per BB.W3c.md:

| Milestone | File created | Same-commit consumer | Atomicity gate |
|---|---|---|---|
| W3c.M1 | `crates/ir/src/rewrites/rank.rs` (~250 LOC) | `crates/ir/src/passes/csp_strategy/mod.rs` extension reading `compute_rank_facts` output | `git log -1 --stat -- crates/ir/src/rewrites/rank.rs` shows the consumer modified in the SAME commit |
| W3c.M2 | `crates/ir/src/rewrites/tiering.rs` (~200 LOC) | same csp_strategy pipeline reading `compute_tiering_facts` output | `git log -1 --stat -- crates/ir/src/rewrites/tiering.rs` shows the consumer modified in the SAME commit |
| W3c.M3 | (extension to existing) `crates/ir/src/passes/recognizers/operator_chain.rs` | extend cost model integration; same commit as Pratt fn emission at `crates/core/src/codegen/rust/emitter/shapes/pratt/struct_direct.rs` | both files modified in SAME commit |
| W3c.M4 | (extension to existing) `crates/ir/src/passes/sets/structural_alphabet.rs` | extend simd-scan kernel selector consumer at `crates/simd-scan/src/lib.rs`; same commit as the structural-alphabet emission | both files modified in SAME commit |

## §3 Consumer wiring sketch (verbatim from BB.W3.md §10)

```rust
// crates/ir/src/rewrites/rank.rs (created at W3c.M1)
use egraph::{Rewrite, RewriteFn, EGraph};
use crate::types::node::IrNode;
use crate::egraph::node::GrammarENode;

/// Rank rewrite: classifies e-graph nodes by structural-rank tier.
/// Consumed by csp_strategy::cost_pipe::extract_with_rank.
pub fn rank_rewrites() -> Vec<Rewrite<GrammarENode, GrammarAnalysis>> {
    vec![ /* rewrite rules */ ]
}

pub struct RankFacts {
    pub tier: u8,
    pub structural_rank: u32,
}

pub fn compute_rank_facts(egraph: &EGraph<GrammarENode, _>) -> Vec<RankFacts> {
    // structural-rank computation over the e-graph saturation result
}

// crates/ir/src/passes/csp_strategy/mod.rs (extended in SAME commit)
use crate::rewrites::rank::{rank_rewrites, compute_rank_facts, RankFacts};
use crate::rewrites::tiering::{tiering_rewrites, compute_tiering_facts, TieringFacts};

pub fn run_csp_strategy(grammar: &mut GrammarIR, egraph: &EGraph<...>) -> StrategyMap {
    let rank_facts = compute_rank_facts(egraph);
    let tier_facts = compute_tiering_facts(egraph, &rank_facts);
    let cost_inputs = CostModelInputs {
        rank: rank_facts,
        tier: tier_facts,
    };
    solve_strategy_csp(grammar, cost_inputs)
}
```

The `compute_rank_facts` produces a `Vec<RankFacts>`; the `run_csp_strategy` reads it; the same commit contains both the producer (`rank.rs`) and the consumer (`csp_strategy/mod.rs`). The atomicity gate is mechanical: `git log -1 --stat` shows both files in the diff.

## §4 Pre-commit verification

The W3c commit body MUST execute the verification before pushing:

```sh
# pre-commit verification per the Era V abrogation
git diff --cached --stat | grep -E 'rewrites/rank\.rs|csp_strategy/mod\.rs' | wc -l
# expects: at least 2 (both files in the staged diff)

git diff --cached --stat | grep -E 'rewrites/tiering\.rs|csp_strategy/mod\.rs' | wc -l
# expects: at least 2 (both files in the staged diff)

cargo nextest run -p bbnf-ir --test substrate_audit
# expects: green; the substrate_audit test enumerates files in rewrites/ + verifies
# each has a caller in passes/

cargo nextest run -p bbnf-ir --test rank_facts --test tiering_facts
# expects: 100% pass; the consumer tests exercise the producer
```

If any verification fails, the commit does not land; the wave halts; the absence of substrate-without-consumer is mechanical, not ad hoc.

## §5 BB.W0a substrate verification

The BB.W0a closer-gate per BB.W0a.md M4:

```sh
test ! -f crates/ir/src/rewrites/rank.rs        # absent at W0a close
test ! -f crates/ir/src/rewrites/tiering.rs     # absent at W0a close
```

The W0a abrogation verifies that the substrate is NOT created prematurely. The W0a verification artefact `W0a-rank-tier-absence.md` records the verification.

If the W0a check fails (the files exist), W0a halts and the precondition for W3c is violated. The check is the structural precondition for W3c's atomic-commit discipline.

## §6 BB.W3c substrate_audit gate

The substrate_audit at `crates/ir/src/passes/tests/substrate_audit.rs` runs at every BB wave's close (per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:226`); the post-W3c verification:

```rust
// crates/ir/src/passes/tests/substrate_audit.rs (extended at W3c)
#[test]
fn no_zero_caller_substrates_post_w3c() {
    let rewrites_dir = "crates/ir/src/rewrites";
    let files = std::fs::read_dir(rewrites_dir).unwrap();
    
    for entry in files {
        let path = entry.unwrap().path();
        if path.extension() == Some("rs".as_ref()) {
            let module_name = path.file_stem().unwrap().to_str().unwrap();
            let callers_in_passes = grep_callers_in_passes(module_name);
            assert!(
                callers_in_passes > 0,
                "substrate {} has zero callers in passes/; Era V failure resurgence",
                module_name
            );
        }
    }
}

fn grep_callers_in_passes(module_name: &str) -> usize {
    // grep -r "use crate::rewrites::<module>" crates/ir/src/passes/ | wc -l
    // ...
}
```

The test fails if any file in `crates/ir/src/rewrites/` has zero callers in `crates/ir/src/passes/`. The 32 zero-caller substrates from BA close (per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:226`) reduce to 0 at BB.W3c close.

## §7 Carry to BC.W0

The BB.W3c artefact `W3c-rank-tier-with-consumer.md` records:
- The atomic commit hashes for W3c.M1 (rank.rs + consumer) and W3c.M2 (tiering.rs + consumer).
- The pre-commit verification output (the `git diff --cached --stat` output showing both files).
- The post-W3c substrate_audit output (zero zero-caller substrates).
- The cost-model integration evidence: `cargo nextest run -p bbnf-ir --test cost_model_pipe` shows the data flow rank → tiering → cost-extractor → CSP-strategy.

BC.W0 entry preflight reads this artefact via the BC entry-preflight check `test -f docs/tranches/BB/audit/W3c-rank-tier-with-consumer.md`. The Era V abrogation is documented for future tranche reference; the same-wave consumer rule becomes precedent for any subsequent rewrite addition.
