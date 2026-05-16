# SK-V7 B2 — CostFacts Substrate Design

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Scope: design-only artifact. No tracked file modified.

## Problem Statement

The CostFacts side-table is the single largest generalization gap in SK-V7.
A6 confirmed (`restart/skinny/audit/SK-V7-COHORT/skv7-A6-ledger-generalization.md`):

- `skinny/crates/passes/src/lib.rs:33-39` writes `layout_facts.backend_shape`
  directly from a single decision tree, with no rejected/dominated alternative
  evidence retained.
- `restart/skinny/COMPILER.md:853-858` (the `cost-model` ledger row) explicitly
  declares the cost model stubbed: no `CostFacts`, no `CostDecision`, no scalar
  score, no Pareto frontier.

A5 (`skv7-A5-lock-audit.md`) catalogued the substrate:

- `BackendShape` enum at `skinny/crates/ir/src/lib.rs:334-341` (five variants:
  `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`).
- `LayoutFacts.backend_shape: HashMap<RuleId, BackendShape>` at
  `skinny/crates/passes/src/lib.rs:62`.
- `derive_backend_shape_with_diagnostics` at
  `skinny/crates/passes/src/lib.rs:287-331`: eight-priority decision tree
  whose steps 6 and 7 are degenerate (`prefers_event_tape` only fires when
  `alt_branch_count >= 8`; the catch-all returns `OffsetTape` without any
  countervailing evidence).
- `skinny/crates/codegen/src/lower/mod.rs:1-9` declares the five per-shape
  lowering modules; only `sink_only` carries a real body (226 LOC). The
  others are five-line diagnostic-string stubs.

REDRESS 72 (cited at `restart/skinny/COMPILER.md:860`) is empirical proof
that per-rule shape and capacity decisions must be CostFacts-recorded, not
hardcoded: a cap-16 tiny-string probe helped `generated-retained` and
regressed `direct` and Track 2. A grammar-wide constant — and a hardcoded
priority tree — cannot encode that asymmetry.

## Design — Types in `ir/`

Place CostFacts in a new module `skinny/crates/ir/src/cost.rs`, re-exported
from `ir/src/lib.rs`. This keeps it adjacent to `BackendShape` and lets
`passes/` and `codegen/` consume it without an extra crate. The module is
grammar-neutral by construction: no JSON-specific variants, no rule-name
literals.

```rust
// skinny/crates/ir/src/cost.rs
use crate::{BackendShape, RuleId};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CostFacts {
    pub rule_id: RuleId,
    pub chosen: BackendShape,
    pub rationale: ShapeRationale,
    pub rejected: Vec<RejectedAlternative>,
    pub priority_fired: PriorityStep,
    pub capacity_policy: Option<CapacityPolicy>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ShapeRationale {
    FirstSetDisjoint,
    FirstSetOverlap,
    ErrorRecoveryRequired,
    HostFnParseTime,
    LayoutScopeWide,
    DirectBuildNoConsumer,
    CollapsedStageAdmissible,
    EventTapeAltDensity,
    DefaultOffsetTape,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum PriorityStep {
    P1_EagerForced,
    P2_SinkOnlyConsumer,
    P3_CollapsedStageNasm,
    P4_EventTapeAltDensity,
    P5_OffsetTapeDispatchable,
    P6_OffsetTapeSpeculative,
    P7_OffsetTapeDefault,
    P8_EagerFallback,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RejectedAlternative {
    pub shape: BackendShape,
    pub reason: RejectionReason,
    pub evidence: Option<Measurement>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum RejectionReason {
    PreconditionUnmet,        // shape's gating predicate did not hold
    InferiorObjective,        // empirically dominated by chosen shape
    AuthorWaiverAbsent,       // CollapsedStage requires per-grammar NASM
    PreviouslyRegressed,      // REDRESS evidence backs the rejection
    ConsumerMismatch,         // SinkOnly without sink, etc.
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Measurement {
    pub workload: String,            // grammar-neutral workload tag
    pub throughput_mbps: Option<f64>,
    pub cycles_per_byte: Option<f64>,
    pub hot_leaf_count: Option<u64>,
    pub source: EvidenceSource,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum EvidenceSource {
    BenchProbe,
    RedressBackfill,
    AuthorDeclared,
    StaticAnalysis,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CapacityPolicy {
    pub tiny_string_cap: u8,
    pub container_initial_capacity: u16,
}
```

`CapacityPolicy` is the carrier for REDRESS-72-class evidence. It lives on
`CostFacts` because cap selection is per-rule (per the REDRESS finding) and
is the second decision the cost model must record beyond `BackendShape`.

`Measurement` is grammar-neutral: `workload` is a free-form tag
(`"generated-retained"`, `"direct"`, `"track2"`), not a JSON corpus name
embedded in the type system.

LOC budget — `ir/src/cost.rs`: ~150 LOC including derives and tests.

## Design — `LayoutFacts` Carrier

Extend `LayoutFacts` (currently `skinny/crates/passes/src/lib.rs:57-63`)
with one field. Keep grammar-specific scaffolds (`hot_call_graph`,
`layout_policies`) adjacent and disjoint from `CostFacts` so neither side
leaks into the other.

```rust
// skinny/crates/passes/src/lib.rs (edit)
use ir::{BackendShape, CostFacts, /* existing */};

pub struct LayoutFacts {
    pub rule_types: HashMap<ir::RuleId, Type>,
    pub node_types: HashMap<ExprId, Type>,
    pub layout_policies: HashMap<String, String>,
    pub hot_call_graph: HashMap<ir::RuleId, recognizers::hot_path::HotPathFact>,
    pub backend_shape: HashMap<ir::RuleId, BackendShape>,
    pub cost_facts: HashMap<ir::RuleId, CostFacts>,    // new
}
```

`backend_shape` is retained as a projection of `cost_facts[rule_id].chosen`
to avoid a churn cascade through the codegen consumers — but the source of
truth shifts. `compile()` populates `cost_facts` first, then derives
`backend_shape` from it (single fold). A debug assertion guards the
invariant: `backend_shape[r] == cost_facts[r].chosen`.

## Design — Producer Refactor

`derive_backend_shape_with_diagnostics` (`passes/src/lib.rs:287-331`) is
refactored into a CostFacts producer. The eight-priority walk is preserved
in form but now records its working state.

```rust
// passes/src/lib.rs (refactored)
pub fn derive_cost_facts(
    grammar: &GrammarIr,
    backend: &BackendIr,
    layout: &LayoutFacts,
    target: TargetFeatures,
) -> CostFactsPlan {
    let mut facts: HashMap<RuleId, CostFacts> = HashMap::new();
    let mut diagnostics: Vec<PassDiagnostic> = Vec::new();

    for rule in &grammar.rules {
        let backend_rule = backend.rules.get(rule.id.0);
        let mut rejected: Vec<RejectedAlternative> = Vec::new();

        let (chosen, rationale, priority_fired) = walk_priorities(
            grammar, backend_rule, layout, target, rule.id,
            &mut rejected, &mut diagnostics,
        );

        let capacity_policy = derive_capacity_policy(rule.id, &rejected);

        facts.insert(rule.id, CostFacts {
            rule_id: rule.id,
            chosen,
            rationale,
            rejected,
            priority_fired,
            capacity_policy,
        });
    }

    CostFactsPlan { facts, diagnostics }
}

pub struct CostFactsPlan {
    pub facts: HashMap<RuleId, CostFacts>,
    pub diagnostics: Vec<PassDiagnostic>,
}
```

The eight priorities, each emitting a `RejectedAlternative` whenever a
higher-precedence shape's gate is checked and fails:

| Step | Gate | Fires shape | Rationale |
|---|---|---|---|
| P1 | `requires_eager_tape` (recovery, host-fn parse-time, layout-wide scope, dispatch overlap) | `EagerTape` | `ErrorRecoveryRequired` / `HostFnParseTime` / `LayoutScopeWide` / `FirstSetOverlap` |
| P2 | `target.direct_only_output && !target.retained_api_consumer && contains_direct_build(rule)` | `SinkOnly` | `DirectBuildNoConsumer` |
| P3 | `target.avx512bw && Entry-shaped && target.collapsed_stage_author_declared` | `CollapsedStage` | `CollapsedStageAdmissible` |
| P4 | `alt_branch_count >= EVENT_TAPE_THRESHOLD` *and* CostFacts records why the threshold value | `EventTape` | `EventTapeAltDensity` |
| P5 | first-set disjoint at every `Alt` site | `OffsetTape` | `FirstSetDisjoint` |
| P6 | first-set overlap admissible (speculative dispatch) — currently a stub | `OffsetTape` | `FirstSetOverlap` |
| P7 | catch-all — currently the silent default | `OffsetTape` | `DefaultOffsetTape` |
| P8 | missing `BackendRule` for the grammar rule | `EagerTape` | diagnostic, no shape |

A5's finding that "steps 6 and 7 are degenerate stubs" is resolved by
making the gate predicate explicit. P6 records `RejectedAlternative` entries
for `OffsetTape`-with-dispatch (P5) and `EagerTape` (P1) — both rejected
because their gates did not hold. P7 records the full alternative roster
with `PreconditionUnmet` so the rule's choice is auditable even when no
higher-precedence gate fires.

The walk is implemented as one pass per priority, short-circuiting on the
first gate that holds. Each gate that does *not* hold appends one
`RejectedAlternative` to the working list. The producer is grammar-neutral:
no rule-name literals, no JSON-shape branches.

`walk_priorities` signature:

```rust
fn walk_priorities(
    grammar: &GrammarIr,
    backend_rule: Option<&BackendRule>,
    layout: &LayoutFacts,
    target: TargetFeatures,
    rule_id: RuleId,
    rejected: &mut Vec<RejectedAlternative>,
    diagnostics: &mut Vec<PassDiagnostic>,
) -> (BackendShape, ShapeRationale, PriorityStep);
```

LOC budget — `passes/src/lib.rs` cost-facts producer: ~200 LOC (net add;
the existing decision tree shrinks by ~40 LOC because predicates move into
named helpers).

### Capacity policy derivation

```rust
fn derive_capacity_policy(
    rule_id: RuleId,
    rejected: &[RejectedAlternative],
) -> Option<CapacityPolicy> {
    // Default policy is None — capacity is shape-default.
    // Producers (REDRESS backfill, samply gate) inject policies through a
    // separate, optional CostHints input. The walk does not invent caps.
    None
}
```

The cost model never invents capacity values; it only records them when
hints carry evidence. REDRESS 72 backfill (below) is the first producer.

## Design — REDRESS 72 Backfill

REDRESS 72 admits a cap-16 tiny-string probe for the generated-retained
plane and rejects the same cap on `direct` and Track 2. This is encoded
as a `RejectedAlternative` on the affected rules' CostFacts. The backfill
runs once at compile time from a fixture under
`skinny/crates/passes/src/redress_evidence.rs`:

```rust
// passes/src/redress_evidence.rs (new)
pub fn redress_72_evidence() -> &'static [(StaticRuleSelector, RejectedAlternative)] {
    // Keyed by rule kind and capacity, not by JSON rule name.
    // StaticRuleSelector matches rules whose BackendExpr carries a
    // tiny-string DirectBuild materializer; CapacityPolicy carries the
    // cap-16 value and the Measurement records the +57.5% Track 1 result
    // and the direct/Track 2 regressions.
    &REDRESS_72_TABLE
}
```

The selector matches on `BackendExpr` shape and `DirectBuildDecode` kind,
not on rule name — preserving grammar neutrality. The backfill emits a
`RejectedAlternative` with `reason: PreviouslyRegressed` and a
`Measurement { source: RedressBackfill, .. }` for every shape the cap-16
probe regressed on, and an `accepted` entry on the matching CostFacts
where the probe helped.

LOC budget — `passes/src/redress_evidence.rs`: ~80 LOC including the
static fixture.

## Design — Codegen Consumption

`codegen/src/lower/` per-shape dispatch reads `cost_facts[rule_id]` rather
than `backend_shape[rule_id]` so the rationale and capacity policy are
available at lowering time. The dispatch surface in
`skinny/crates/codegen/src/lower/mod.rs` grows one helper:

```rust
// codegen/src/lower/mod.rs (edit)
pub fn select_lowering<'a>(
    cost: &'a CostFacts,
) -> &'a dyn ShapeLowering {
    match cost.chosen {
        BackendShape::EagerTape       => &eager_tape::Lowering,
        BackendShape::OffsetTape      => &offset_tape::Lowering,
        BackendShape::EventTape       => &event_tape::Lowering,
        BackendShape::SinkOnly        => &sink_only::Lowering,
        BackendShape::CollapsedStage  => &collapsed_stage::Lowering,
    }
}

pub trait ShapeLowering {
    fn lower_rule(
        &self,
        ctx: &mut LowerCtx,
        rule: &BackendRule,
        cost: &CostFacts,
    ) -> Result<TokenStream, LowerError>;
}
```

Each shape lowerer receives the `CostFacts` for the rule it is lowering.
`sink_only::Lowering` (the only non-stub today) consumes
`cost.capacity_policy` for tiny-string cap selection. Stubs gain a
`debug_assert!(matches!(cost.chosen, BackendShape::_))` to keep the
dispatch invariant honest.

LOC budget — `codegen/src/lower/` surfacing: ~50 LOC across all five
lowerers plus `mod.rs`.

## Design — Diagnostics

`passes/src/diagnostics.rs` already declares
`BBNF-BACKEND-SHAPE-INCONSISTENT` and `BBNF-COLLAPSEDSTAGE-NOT-VIABLE`.
Two new codes:

```rust
// passes/src/diagnostics.rs (edit)
pub enum PassDiagnosticCode {
    CollapsedStageNotViable,
    BackendShapeInconsistent,
    DominatedAlternative,        // new
    CostFactsMissingEvidence,    // new
}

impl PassDiagnosticCode {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::CollapsedStageNotViable     => "BBNF-COLLAPSEDSTAGE-NOT-VIABLE",
            Self::BackendShapeInconsistent    => "BBNF-BACKEND-SHAPE-INCONSISTENT",
            Self::DominatedAlternative        => "BBNF-DOMINATED-ALTERNATIVE",
            Self::CostFactsMissingEvidence    => "BBNF-COSTFACTS-MISSING-EVIDENCE",
        }
    }
}
```

`BBNF-DOMINATED-ALTERNATIVE` fires when a `RejectedAlternative` with
`PreviouslyRegressed` evidence is for a shape that, under the current
target, would pass its gate. This is the warning the cost model emits when
the world changes underneath a REDRESS finding.

`BBNF-COSTFACTS-MISSING-EVIDENCE` fires when a rule's CostFacts carries
fewer than `MIN_REJECTED_FOR_AUDIT` (4) alternatives — guarding against a
regression to silent defaults.

`BBNF-BACKEND-SHAPE-INCONSISTENT` (already declared in
`restart/ARCHITECTURE.md` §7.5) now carries CostFacts evidence in its
message body, citing the rejected alternative whose gate the producer
believed should have held.

## Design — xtask gate-json Surface

The xtask `gate-json` binary (under `skinny/xtask/`) emits a JSON report
keyed by rule. Today the report carries `backend_shape` only; the SK-V7
extension serializes the full CostFacts table:

```json
{
  "grammar": "json",
  "cost_facts": {
    "5": {
      "rule_id": 5,
      "chosen": "OffsetTape",
      "rationale": "FirstSetDisjoint",
      "priority_fired": "P5_OffsetTapeDispatchable",
      "rejected": [
        {
          "shape": "EagerTape",
          "reason": "PreconditionUnmet",
          "evidence": null
        },
        {
          "shape": "SinkOnly",
          "reason": "ConsumerMismatch",
          "evidence": null
        },
        {
          "shape": "CollapsedStage",
          "reason": "AuthorWaiverAbsent",
          "evidence": null
        },
        {
          "shape": "OffsetTape",
          "reason": "PreviouslyRegressed",
          "evidence": {
            "workload": "track2-cap16",
            "throughput_mbps": null,
            "cycles_per_byte": null,
            "hot_leaf_count": null,
            "source": "RedressBackfill"
          }
        }
      ],
      "capacity_policy": { "tiny_string_cap": 16, "container_initial_capacity": 4 }
    }
  }
}
```

Because `CostFacts` and its sub-types derive `Serialize`, the gate-json
serializer is mechanical. The xtask gate adds a single `--with-cost-facts`
flag (default on for SK-V7); when the report is consumed by CI, the
falsifiability gate runs over the serialized table.

LOC budget — `skinny/xtask/src/bin/gate_json.rs`: ~80 LOC for the new
serialization branch and the `--with-cost-facts` flag plumbing.

## Implementation Sequence

| Step | Owner | Files | LOC | Gate |
|---|---|---|---|---|
| 1 | ir | `ir/src/cost.rs`, `ir/src/lib.rs` re-export | 150 | type-only |
| 2 | passes | `passes/src/lib.rs` refactor + `redress_evidence.rs` | 280 | `cargo test -p passes` |
| 3 | passes | `LayoutFacts.cost_facts` field; `compile()` populates | included above | `debug_assert` shape parity |
| 4 | codegen | `lower/mod.rs` trait + per-shape consumers | 50 | `cargo test -p codegen` |
| 5 | xtask | gate-json `--with-cost-facts` | 80 | golden gate-json fixture |
| 6 | docs | `ARCHITECTURE.md` §7, `COMPILER.md` cost-model row | 150 | doc review |
| **Total** | | | **~830 LOC** | |

Step 1 lands as a leaf commit — no consumer changes. Step 2 is the
producer refactor and is the only step that touches the existing decision
tree at `passes/src/lib.rs:287-331`. Steps 4 and 5 are independently
mergeable once step 2 is in. Step 6 lands last so the doc tree never
quotes types that do not exist.

The full sequence sits in **Wave 1 of SK-V7**, after Wave 0
(`comparator-plane repair`) and before Wave 2 (`per-shape lowerer bodies`).
CostFacts must precede the lowerer bodies because the per-shape lowerers
in step 4 consume `CapacityPolicy` and `RejectedAlternative` for emission
decisions — without CostFacts, the lowerer rewrites would re-invent the
hardcoded thresholds the substrate is meant to eliminate.

## Falsifiability Gate

After the substrate lands, the SK-V7 Wave 1 exit gate enforces:

1. Every rule appearing in `shapes_for_json()` (currently 7 — `JsonRoot`,
   `JsonValue`, `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`,
   `JsonNumber`, `JsonBool`, `JsonNull`) has a populated `CostFacts`
   entry. (Note: this gate uses the existing prototype roster as a count
   floor; the long-term replacement is `DirectFieldFacts` per A5 / C6.
   The cost gate is roster-shape-agnostic — it asserts on rule count, not
   rule name.)
2. Every CostFacts entry carries at least four `RejectedAlternative`
   entries (one per non-chosen `BackendShape`).
3. Every REDRESS-evidenced rejection (currently REDRESS 72) appears in at
   least one CostFacts entry with `EvidenceSource::RedressBackfill`.
4. `xtask gate-json --with-cost-facts` round-trips through serde without
   information loss (proves the side-table is fully exported).
5. No CostFacts entry uses `ShapeRationale::DefaultOffsetTape` without a
   `BBNF-COSTFACTS-MISSING-EVIDENCE` diagnostic for the same rule. Silent
   defaults are forbidden.

The gate runs against `skinny/crates/runtime/src/grammars/json/generated.rs`
generation and fails the build if any of the five conditions hold.

## Grammar-Neutrality Checklist

- `CostFacts` and `ShapeRationale` carry no JSON-specific variants.
- `RejectionReason` and `EvidenceSource` are predicate categories, not
  workload labels.
- `Measurement.workload` is a free-form string — no `JsonCorpus` enum.
- `CapacityPolicy` fields are unitless caps. JSON-shape-specific caps live
  on the `BackendExpr` selectors in the REDRESS evidence table, not in the
  `CapacityPolicy` type.
- `redress_evidence.rs` selectors match on `BackendExpr` and
  `DirectBuildDecode` kinds, not on rule names. The same selectors work
  for a future CSS grammar without edit.
- `cost_facts` lives at the same level in `LayoutFacts` as
  `hot_call_graph`; no nesting between grammar-specific scaffolds and the
  grammar-neutral cost record.

## Single Biggest Design Risk

**Producer drift between the decision walker and the CostFacts record.**
The eight-priority walker (`walk_priorities`) must, for every priority it
checks, append a `RejectedAlternative` whose `reason` matches the gate it
just evaluated. If a future edit adds a priority predicate but forgets to
append the rejection, the falsifiability gate (rule 2: four rejections per
entry) keeps firing, but the recorded rationale becomes silently
incomplete — every chosen shape still has an entry, but the audit trail
diverges from the actual decision logic. Mitigation: encode the priority
table as a `&'static [PriorityStep]` plus a `gate_fn` per step, so the
walker is a fold over the table and adding a step requires adding both
the gate and its rejection-emission together. The `PriorityStep` enum
variant count is the static check that the table is exhaustive.

The secondary risk is `CapacityPolicy` shape itself: REDRESS 72 covers
`tiny_string_cap` and `container_initial_capacity`, but the substrate
must not freeze just those two fields. A future REDRESS on, e.g.,
event-tape ring sizing would force a struct edit. Holding the policy as a
small bag (`Option<HashMap<&'static str, u32>>`) was considered and
rejected: untyped policy carriers are exactly the substrate erosion the
CostFacts design is meant to prevent. Edits to `CapacityPolicy` are
expected and acceptable.
