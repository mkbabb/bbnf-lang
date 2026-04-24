# Tranche BB — E-graph Rewrite Rule Inference

BB closes the loop on `feedback_pluggable-components`,
`feedback_csp-always-optimize`, and `feedback_general-infra-crates`
by letting the e-graph *discover* grammar-level rewrite rules rather
than only apply a fixed set. Ruler-style CVC enumeration over
`IrNode` produces candidate pairs `(L, R)`; the e-graph itself is
the fast-path equivalence check; the surviving VM interpreter
serves as the non-circular ground-truth oracle on the *residue* —
candidates the e-graph cannot decide — and nothing more. An
automatic ranker scores every surviving candidate; a tiered review
pipeline auto-accepts the trivial class, fast-tracks the structural
class, and reserves full human review for the novel class only.
Rules live outside `crates/core`: fleet-wide rules in a new
`crates/ir/src/rewrites/` module, grammar-specific rules
colocated with each grammar under `grammar/<name>/rewrites/*.ron`
via a standardised schema the `bbnf_derive` build step compiles
into that grammar's cost-config.

## Preflight truth from 2026-04-24

The probability-lift pass found that BB's planned storage and
enumeration surfaces are not code-real yet:

- `crates/ir/src/rewrites/` does not exist.
- `crates/egraph/src/ruler/` does not exist.
- The live rewrite path is fixed Rust under
  `crates/ir/src/egraph/rules/`.
- No RON rule schema, `RuleSet`, provenance type, grammar-colocated
  rewrite discovery, derive cache invalidation, or VM-residue oracle
  wrapper exists yet.
- Historical e-graph evidence says many fixed rules did not fire on
  production grammars; BB must prove fire/extract/writeback/emission
  before it claims performance impact.

BB therefore opens on a substrate preflight, not on enumeration.
The first close condition is "a discovered or stored rule changes
generated parser code through the live pipeline", not "a rule file
exists".

## Architectural thesis

1. **Rule inference over `IrNode` is an e-graph enumeration
   problem.** Ruler (Nandi et al. 2021) demonstrates the CVC-style
   approach: generate candidate terms over the grammar's alphabet
   up to bounded size, group them by equivalence under an oracle,
   extract rules as cross-class equivalences. BB applies the same
   shape to bbnf's `IrNode`.
2. **The e-graph is the fast path; the VM is the residue oracle.**
   An e-graph that already contains both `L` and `R` in the same
   class proves their equivalence without any external call. The
   VM runs only when the e-graph is silent — i.e., `L` and `R`
   belong to different classes under the current rewrite set.
3. **Rules are grammar-derived, not hand-coded.** Tranche H's
   factor / merge_regex_alts / inline_acyclic were hand-written.
   BB does not hand-code; BB discovers. Every rule that persists
   was produced by enumeration, survived oracle validation, and
   cleared the ranker tiering.
4. **Storage is extensible and out-of-core.** Fleet-wide rules
   live in `crates/ir/src/rewrites/`; grammar-specific rules colocate
   with their grammar directory. `crates/core` never accumulates a
   hand-curated rule list. Adding a grammar does not require
   editing core.
5. **Ranking + tiering is first-class.** Candidates are scored on
   frequency, cost delta, generality, similarity-to-known, novelty,
   and size. Class 1 (trivial / algebraic / rediscovered) auto-
   accepts with audit log only. Class 2 (structural resemblance
   to hand-coded patterns) fast-tracks. Class 3 (novel) is the
   only class that consumes human review time.

## Rule admission chain

A rule is not admitted until the whole consumer chain is green:

```text
RON rule or inferred candidate -> schema validation -> derive cache key
-> live rule registry -> e-graph search/apply -> extraction chooses it
-> write_back_optimized changes GrammarIR -> expanded Rust hot path changes
-> fixture and bench/proof move
```

Per-rule report must show `search > 0`, `apply/work > 0`, extraction
selected the new form, and generated code changed in a parser hot path.
Line-count-only diffs, rule-store-only diffs, and rules subsumed by the
pre-egraph normalizer do not close a BB wave.

Command packet:

```bash
rg -n 'src/rewrites|src/ruler|RuleSet|Provenance|rewrites/' crates/ir crates/egraph crates/derive grammar

BBNF_EGRAPH_REPORT=1 BBNF_HIR_EGRAPH_REPORT=1 \
cargo run -p bbnf --example egraph_fire_probe --profile ax-iter

BBNF_PIPELINE_REPORT=1 BBNF_EGRAPH_REPORT=1 BBNF_CSP_REPORT=1 \
cargo expand -p bbnf --bench json_monolithic > target/expand/bb-json.rs
```

Profile only after the expand diff proves emitted hot-path change.

## Architecture — e-graph first, VM residue second

The question "why is the VM being used for an oracle equivalence
when our e-graph system should do the same thing?" is sharper than
it looks. The answer is that the e-graph alone cannot *bootstrap* —
before any rule is proven, every candidate pair is in its own
singleton class and the e-graph proves nothing. The VM supplies
the extrinsic ground truth that seeds the first round; the e-graph
then amortises every subsequent check against the growing rewrite
set.

Flow per enumeration pass:

```
                        candidate (L, R)
                              │
                              ▼
        ┌────────────────────────────────────┐
        │  e-graph equivalence check under   │
        │  current accepted rewrite set      │
        └────────────────────────────────────┘
                    │              │
          same class│              │different classes
                    ▼              ▼
            skip (already      VM oracle: run L, R
            captured or        over fixture corpus;
            redundant)         compare tape output
                                      │
                               ┌──────┴──────┐
                               │             │
                         equivalent      diverges
                               │             │
                               ▼             ▼
                        feed into       discard
                        ranker
                               │
                               ▼
                     tiered review
                               │
                               ▼
                     persist accepted
                     rules; next pass
                     sees them in
                     the e-graph
```

Three consequences:

1. **The residue is small.** Empirically in Ruler / Enumo, >90% of
   candidate pairs are captured or redundant once even a small
   seed ruleset is in place. BB's VM workload is sized to the
   residue, not total enumeration.
2. **The VM stays narrow.** Its job is: compile `L` and `R` to the
   bytecode already supported at HEAD (`crates/ir/src/vm/`,
   ~1800 LOC), run each against the fixture corpus, byte-compare
   tape output. No DTA walker. No shape emitter. No resurrection
   of the dispatch machinery that AX.W0b abrogated.
   `feedback_abrogate-before-patch` holds: we consume what
   survived, we do not re-open what was deleted.
3. **Each round compounds.** Accepted rules extend the e-graph's
   rewrite set. The next enumeration pass's residue is strictly
   smaller — rules the e-graph now proves equivalent for free
   stop reaching the VM. Convergence is observable in residue
   size per pass.

The VM's oracle role is scoped to residue only. The walker that
AX.W0b deleted stays deleted; we consume only the narrow VM
surface that compiled at HEAD through the rename.

## Storage architecture

Rules do not live in `crates/core`. Two layers:

### Fleet-wide rules — `crates/ir/src/rewrites/`

A module inside the existing `bbnf-ir` crate, per
`feedback_no-core-dumping`. A standalone `ir-rewrites` crate was
explicitly rejected: rewrite rules operate on `IrNode` shapes and
are not general-purpose infrastructure the way `bbnf-egraph` or a
cost-model crate would be, so they do not merit an independent
crate boundary. `feedback_general-infra-crates` applies to the
e-graph machinery at `crates/egraph/` and the enumeration
scaffold, not to the rule store. The module contains:

- `mod.rs` — `Rule` schema, `RuleSet` registry, provenance types.
- `base/*.ron` — the base rules shipped with bbnf: `Concat(x,
  Epsilon) → x`, `Alt(Alt(a,b), c) → Alt(a, b, c)`, identity
  folds, bounded unrolling, etc. Every file is a plain data file
  — no Rust authorship required to add a fleet-wide rule.
- `rank.rs` — the automatic ranker (see next section).
- `tiering.rs` — Class-1 / Class-2 / Class-3 classifier.
- `schema.rs` — rule-file validation against the schema.

### Grammar-specific rules — `grammar/<name>/rewrites/*.ron`

Each grammar directory may contain a `rewrites/` subdirectory.
Every `.ron` file there declares one rule in the standardised
schema:

```ron
Rule(
    name: "css_declaration_flatten_trailing_semicolons",
    lhs: Concat([
        IrNode::Sym("declaration"),
        IrNode::Repeat(box IrNode::Terminal(";")),
    ]),
    rhs: IrNode::Sym("declaration"),
    cost_delta: -3,
    provenance: Inferred(
        enumeration_run: "2026-05-02-css-w2",
        corpus_coverage: 0.97,
    ),
    tier: Class1,
)
```

At build time the `bbnf_derive` macro scans `grammar/<name>/
rewrites/` for every grammar it processes, parses each rule file
against the schema, and compiles matching rules into that grammar's
`cost_config`. Fleet-wide rules are statically linked from
`crates/ir/src/rewrites/`. Grammar authors add rules by dropping files
in — no core edit, no derive edit.

### Why RON rather than `.rs`

RON is data-only and schema-validatable at build time; rule files
are data, not code. A typo in a RON file is caught by the schema
validator with a file/line pointer. A typo in an `.rs` rule file
is caught by `rustc` but drags the full macro surface into each
rule. Data-only wins.

### Extensibility statement

Adding a new grammar `foo` with three custom rewrites requires:
creating `grammar/foo/rewrites/{r1,r2,r3}.ron`. That is the whole
delta. No `crates/core` edit. No `bbnf_derive` edit. No hand-
authored registry. This is what "grammar-colocated, modular,
extensible" resolves to.

## VM-as-oracle

The VM role is explicit and narrow:

1. **Input**: two `IrNode` candidates `L`, `R` plus the grammar's
   fixture corpus.
2. **Compile**: each candidate is lowered to the existing
   bytecode surface (`crates/ir/src/vm/bytecode.rs`). No new
   opcodes. Lowering is a restricted subset — only node kinds
   the enumerator produces.
3. **Execute**: both bytecode programs run against every fixture
   in the corpus under a time + memory budget per candidate.
   Outputs are tape sequences.
4. **Compare**: byte-equal tape output across the full corpus ⇒
   equivalent. Diverges on any fixture ⇒ not equivalent. Timeout
   on any fixture ⇒ inconclusive, queued for wave-close review,
   never auto-shipped.

The VM is ~1800 LOC at HEAD. BB neither grows it nor revives the
token-dispatch walker dispatch; the walker stayed deleted at
AX.W0b and stays deleted here. Oracle-only.

## Ranker + tiered review

Every candidate surviving the oracle is scored.

| Signal | Measures | Why it matters |
|---|---|---|
| Match frequency | # of corpus matches for LHS pattern across all grammars | Rules that never fire have no value |
| Cost delta | `cost(LHS) - cost(RHS)` via existing cost model | Larger reduction ⇒ higher priority |
| Generality | # of grammars whose IR contains LHS pattern | Fleet-wide wins outweigh single-grammar wins |
| Similarity-to-known | Structural edit distance to Tranche H hand-coded rules | Near-matches are low-risk |
| Novelty | Inverse of similarity | Flags for scrutiny, not a penalty |
| Tree size | LHS node count | Smaller LHS easier to verify, less fragile |

The weighted score feeds a tier classifier:

- **Class 1 — Trivial.** Algebraic identities, associativity /
  commutativity already present in the ground-truth set,
  rediscoveries of Tranche H hand-coded rules. Auto-accept with
  machine-generated justification committed to the audit log.
  No human gate.
- **Class 2 — Structural.** Shape resembles a Tranche H hand-coded
  pattern but differs in arity, ordering, or a literal. Fast-track
  review: one-line LGTM plus corpus coverage assertion. The review
  surface is a Markdown table plus a per-rule Markdown doc under
  `docs/rules/<rule-id>.md` generated by the ranker.
- **Class 3 — Novel.** High novelty score, no structural kin in
  the Tranche H set. Full human review: rationale, corpus-coverage
  audit, intentional-divergence probing (i.e., do we *want* this
  transformation semantically?). The only class that burdens review
  time.

Target distribution: >90% Class 1 + Class 2, <10% Class 3. Human
review time scales on novel rules only. The bootstrap review is
bounded by the Tranche H ground-truth rule count (low dozens);
post-bootstrap the delta review is per IR or cost-model change —
incremental and small.

### Review surface — where rules are saved and how humans process them

- **Saved**: per the storage architecture above — fleet-wide
  inferred rules land in `crates/ir/src/rewrites/inferred/*.ron`;
  grammar-specific inferred rules land in
  `grammar/<name>/rewrites/*.ron`.
- **Machine output per run**: one Markdown report per enumeration
  run under `docs/rules/runs/<run-id>.md`, plus one
  `docs/rules/<rule-id>.md` per candidate rule. Reports include
  the ranked candidate list, tier assignment, cost delta,
  provenance, corpus coverage, and rationale.
- **Reviewer workflow**: sort by tier; confirm Class 1 audit log
  passes; fast-track Class 2 in batches; deliberate on Class 3
  one at a time. Accepted rules are committed as RON files;
  rejected rules are recorded under
  `docs/rules/rejected/<rule-id>.md` with rationale.

## Dependencies

BB depends on **AZ-I close + AY-II close**. BA (lazy pointer queries)
is **not** a BB blocker — rule inference operates over `IrNode`,
not over pointer-path output, so BB can run in parallel with BA
or after BA.

**AZ-I close dependencies:**

- Direct-to-struct substrate stable. The enumerator relies on the
  struct tree as the canonical target form; the oracle compares
  tape output produced by the struct-aware emitter.
- Tape abrogation complete. The VM compares materialised struct
  state where tape is absent, and tape output where tape remains.

**AY-II close dependencies:**

- Projection truth. Every `->` reaches the tape emitter, giving
  the oracle a canonical comparison point across grammars.
- Unified compound emission API. The VM oracle composes against
  the same primitives every grammar uses.

## Hard gates

**Rule-inference gates:**

- Each wave declares N_w candidates and ships an oracle run that
  validates them. `W0 = 0 (scaffold only); W1 = 20 JSON; W2 = 50
  CSS + BBNF; W3 = grammar-specific corpora`.
- Oracle rejection rate ≤ 50% per wave; higher rejection indicates
  alphabet drift — the alphabet narrows before the next wave.
- **Soundness rediscovery**: the enumerator must rediscover at
  least 80% of Tranche H's hand-coded rules on matching grammars.
  A miss there is a soundness bug, not a coverage gap.

**Ranker gates:**

- ≥ 90% of accepted rules classify as Class 1 or Class 2 across
  all shipped waves.
- Class 1 auto-accept has a signed audit-log entry in
  `docs/rules/audit-log.ndjson` for every rule.
- Class 3 rules have a `docs/rules/<rule-id>.md` doc with
  reviewer-signed rationale.

**Cost gates:**

- After applying accepted rules, at least one grammar's
  `generated.rs` shrinks by ≥ 10 LOC.
- At least one grammar shows a measurable throughput gain on the
  17-entry AU-baseline matrix.
- No grammar regresses on the 17-entry matrix.

**Parity gates:**

- lightningcss / sonic-rs / simdjson parity harnesses green after
  every accepted rule.
- Workspace: pass count ≥ AZ-I close, fail count ≤ AZ-I close.

**Storage gates:**

- `crates/ir/src/rewrites/` compiles standalone; schema validator
  rejects malformed RON with file/line diagnostics.
- `bbnf_derive` discovers and compiles `grammar/<name>/rewrites/
  *.ron` for every grammar without per-grammar edits.

## Wave structure

Five waves. Every wave has a runtime call site at its landing
commit. Each wave spec is ≤ 150 LOC.

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Enumerator + VM oracle + ranker + `crates/ir/src/rewrites/` scaffold; Tranche H soundness rediscovery | AZ-I + AY-II close | planned |
| **W1** | [waves/W1.md](waves/W1.md) | First enumeration run — JSON + Sheets, curated Class-1/2 batch | W0 | planned |
| **W2** | [waves/W2.md](waves/W2.md) | Wide alphabet — CSS L4 + BBNF self-hosting | W1 | planned |
| **W3** | [waves/W3.md](waves/W3.md) | Grammar-specific rule discovery + per-grammar `rewrites/*.ron` authoring | W2 | planned |
| **W4** | [waves/W4.md](waves/W4.md) | FINAL — cost-model integration + CI auto-accept + review-ledger close | W3 | planned |

## Reversal criteria

Inheriting AZ-I's discipline:

1. **Wave-local 20% rule.** A wave whose accepted rules fail to
   produce the declared cost delta by > 20% of target reverts its
   rule batch.
2. **Soundness violation ⇒ immediate revert.** A rediscovered rule
   that fails the corpus is a soundness bug in the enumerator or
   oracle; wave halts, batch reverts, root cause found before the
   next run opens.
3. **No regression on AZ-I / AY-II close.** Any regression of the
   17-entry matrix reverts the responsible rule batch.
4. **No hedging forward.** A wave does not route its miss to a
   later wave of BB.
5. **E-graph node-count ceiling.** Crossing the per-wave ceiling
   reverts the enumeration alphabet at wave close; alphabet
   narrows before the next wave opens.

## Risk register

| Risk | Mitigation |
|---|---|
| **E-graph explosion** — candidate enumeration blows past the bounded budget and consumes GB of memory. | Per-wave size bound; fail-fast if `EGraphSolver::node_count()` exceeds the declared ceiling. Revert the enumeration alphabet. |
| **Rule validity drift** — a rule is equivalent on the fixture corpus but semantically wrong on an unseen input. | Accept rules only with oracle coverage ≥ 95% of fixture bytes. Rules with narrower coverage ship as `narrow` and restrict to matching grammar positions. |
| **Interaction with AY-II fusion** — a rule rewrites a fused form back into an unfused form, regressing throughput. | The cost model is the ranker's gate: a rule with negative cost delta cannot auto-accept. |
| **Oracle timeout storms** — enumeration emits many candidates the VM cannot validate in budget. | Per-candidate timeout marks "inconclusive"; inconclusives never auto-ship and are reviewed at wave close. |
| **Rule accumulation entropy** — hundreds of accepted rules slow compilation. | Hard cap of 25 accepted per grammar; new accepts must displace older accepts on measured cost delta. |
| **Ranker miscalibration** — too many candidates land in Class 3. | Ranker weights are themselves subject to review at wave close; W4 includes a calibration audit. |

## Critical files

| File | Status | Role |
|---|---|---|
| `crates/egraph/src/ruler/enumerate.rs` | create | Ruler-style CVC enumerator |
| `crates/egraph/src/ruler/oracle.rs` | create | VM oracle wrapper with per-candidate budget |
| `crates/egraph/src/ruler/residue.rs` | create | E-graph-first check; routes residue to oracle |
| `crates/ir/src/rewrites/` | create (new module inside `bbnf-ir`) | Fleet-wide rule registry, schema, ranker, tiering |
| `crates/ir/src/rewrites/mod.rs` | create | `Rule`, `RuleSet`, provenance types |
| `crates/ir/src/rewrites/rank.rs` | create | Automatic ranker |
| `crates/ir/src/rewrites/tiering.rs` | create | Class-1/2/3 classifier |
| `crates/ir/src/rewrites/schema.rs` | create | RON rule-file schema + validator |
| `crates/ir/src/rewrites/base/*.ron` | create | Base fleet-wide rule files |
| `grammar/<name>/rewrites/*.ron` | create per grammar | Grammar-specific rule files |
| `crates/bbnf_derive/src/rewrites.rs` | create | Build-time scan + compile of rule files into cost-config |
| `docs/rules/` | create | Per-rule docs, run reports, audit log |

## Defensible floor

Minimum BB delivers:

1. Working Ruler-style enumerator + e-graph residue split + VM
   oracle wrapper.
2. Automatic ranker with Class-1/2/3 tiering functional.
3. `crates/ir/src/rewrites/` module landed inside `bbnf-ir` with
   base rules and schema validated.
4. JSON grammar: ≥ 5 accepted rules, auto-accept on Class 1, review
   on Class 2 + 3, measurable codegen shrink ≥ 10 LOC on JSON.
5. Tranche H ground-truth rules rediscovered by enumeration on
   matching grammars (soundness check).
6. No regression on AZ-I close 17-entry matrix; parity harnesses
   green.

Other grammars, novel-rule review, and per-grammar
`grammar/<name>/rewrites/` authoring are stretch beyond the floor
but shipped in W2–W3 under normal execution.

## External SOTA grounding

- **egg — equality saturation with e-graphs.** The substrate bbnf
  uses today (`crates/egraph/`). See
  [egg home](https://egraphs-good.github.io/) and the
  [egg SIGPLAN blog](https://blog.sigplan.org/2021/04/06/equality-saturation-with-egg/).
- **Ruler — rewrite rule inference.** The technique BB applies.
  CVC-style enumeration up to bounded term size, pairwise
  equivalence checks under an oracle, cost-ranked rule extraction.
  See [Rewrite Rule Inference (Nandi et al. 2021)](https://arxiv.org/pdf/2108.10436).
- **Enumo — follow-on tooling for rule inference.** Newer
  infrastructure from the same line of research with support for
  conditional rules and domain-specific rulers. See
  [Enumo paper](https://dl.acm.org/doi/10.1145/3591283).

## Indefatigability

When BB closes correctly, bbnf's optimiser discovers rules as
first-class output; the e-graph is the fast-path proof substrate
and the VM is the bounded ground-truth oracle on residue; rules
live outside core, fleet-wide in `crates/ir/src/rewrites/` and
per-grammar in `grammar/<name>/rewrites/`; an automatic ranker
plus tiered review keeps human attention on novel rules only; the
DTA/PSI era's surviving VM interpreter finds permanent purpose as
the equivalence-residue oracle without any resurrection of the
walker substrate that Era V could not make pay.
