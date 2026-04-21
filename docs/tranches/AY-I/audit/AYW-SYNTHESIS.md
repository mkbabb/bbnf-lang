# AY Synthesis — Six-Agent Audit Consolidation + Revised Continuation

Authored 2026-04-20 at master HEAD `a9efb966`. Synthesises six parallel
read-only audits:

1. `AYW-retrospective-wiring.md` — substrate-with-consumer activation (20
   ACTIVE / 5 SHIPPED / 2 DEAD / 1 REGRESSED).
2. `AYW-csp-scope.md` — CSP solver scope + full-generality review.
3. `AYW-egraph-cost-firing.md` — 13 of 14 e-graph rules dead on
   production grammars.
4. `AYW-structural-analysis-gap.md` — 8,114 LOC classifier; 4×
   duplicated FIRST-sets; ~1,450 LOC retirable.
5. `AYW-archaeology-optimization-arc.md` — 18-tranche optimization
   trajectory; AU historical peak; recurring anti-patterns.
6. `AYW-dev-expedite.md` — 12–15 min cold cycle → 45–90 s achievable.

## I. The converging finding

AY ships **architectural substrate faster than consumers can activate it**.
The six audits, each sampled a different substrate surface, all surface
the same fault: consumer-without-substrate is rare; **substrate-without-
consumer is endemic**.

Quantitatively across AY:

- E-graph rule set: **13 of 14 rules register zero fires** on production
  grammars. The normalizer (`inline` / `merge_literals` /
  `factor_common_prefixes` / `eliminate_epsilon`) converges before the
  e-graph runs; its canonical form already subsumes what G1-G4 +
  CommonSuffixFactor + HIR-tier rules would rewrite.
- `crates/core/src/runtime/handle.rs` (W3a.1, 142 LOC): **zero emitted
  consumers**. `materialize_*` fns walk views via `span_text()` /
  `payload_f64` accessors; `StringHandle` + `CompoundHandle` never appear
  in generated bodies.
- `crates/core/src/generate/regex/phf.rs` (W4.3): **zero workspace
  callers**. Scaffold-without-consumer.
- `tape::structural_scan` (W1.3): REGRESSED eagerly (−64 % twitter;
  W1-fix retired call) → LAZY via W4.3's CTNS probe on Sheets only → no
  measurable gain anywhere.

And the work *is* cohesive at the CSP / cost-model substrate:

- `egraph::CostWeights` (10 knobs) is authoritative; `CostModel<N>`
  trait pluggable; regex tier already embeds same weights. Unification
  already present at substrate — just under-wired at two consumers
  (`byte_class.rs` + `payload/layout.rs`).
- Nine CSP consumers share one `Csp` surface (`unified-propagate`
  holds). Three heuristics (payload-layout greedy, e-graph greedy
  extract, shape-dict stub `install`) would be natural CSP consumers
  but aren't.

The problem isn't the substrate architecture. It's that AY **added
surface** (structural-scan, G1-G4, phf.rs, handle.rs, materialize_*
fns) **before retiring the competing surfaces it superseded**. The
result is triple-path: the normalizer, the e-graph, and the classifier
all answer overlapping questions with stale boundaries between them.

## II. AU-era regression chain — confirmed

Archaeology §4 confirms the three AU-era invariants that eroded:

1. **Flat AoS `Vec<TapeRec>` → SoA 7-column pivot** (AV/AW-I). 7 Vec
   pushes per push; `push_structural` hit 23-43 % self-time. AY.W1
   reverted correctly. **bytes/cyc on twitter: AU 0.615 → pre-AY 0.137
   → AY.W1-fix 0.215.** Half-restored.
2. **Unified `push_leaf_with(kind, PayloadData)` → multi-column
   `push_structural` cross-crate call**. AY.W1.5 `#[inline(always)]`
   closed this structurally (nm confirms zero cross-crate exports);
   samply confirms the LTO-collapsed body dominates at 55 % of
   parse-time, which is the new architectural ceiling.
3. **`.map(|_|()) → compound-wrap on scalar leaves`** (AW-V.W4).
   Record count: 100 K scalars become 200 K records; sonic-rs has no
   wrap. AY.W2.6 elides wrap on 3 BBNF rules (−9 % record count).
   **Full restoration requires grammar-shape-specific elision that
   production JSON's `Alt(Ref, Ref, …)` does not match** (G3's
   precondition).

## III. What AY's remaining hot-path owners actually are

Per `.profiles/samply/post-AY-W1-fix/json_monolithic/twitter/top10-self-time.txt`:

- **55.19 %** `<JsonParser>::parse` — the LTO-inlined parse body.
  Contains inline push, bitmap scan, dispatch. Single monolithic
  frame; optimisation here demands emitter changes, not substrate
  additions.
- **24.12 %** `parse_object_JsonParser_object` — per-rule body.
- **11.81 %** `parse_wrap_JsonParser_value` — wrap-emitter overhead
  (the 50 % record-count projection that AY.W2.6 didn't meet).
- < 3 %: everything else combined. `tape::structural_scan::*`,
  `push_leaf_*`, `finalise::*`: all ≤ 1 % (substrate verdicts green by
  nm + samply).

The BEAT-sonic gap is not in the tape substrate. It is in the
**emitted parse body**. sonic-rs's `from_str::<Value>` is ~238 µs on
twitter; our parse + materialize is ~864 µs. The 3.63× ratio decomposes
approximately:

- Parse only (no Value): ~890 MB/s bbnf vs sonic unknown but
  ≥ 1967 MB/s. So even ignoring Value, bbnf is ~2.2× slower on pure
  parse.
- Value materialization adds ~120 µs on bbnf (the `to_value()` view
  walk). sonic-rs interleaves parse + materialize in one pass.

The path to BEAT-sonic runs through **fused parse+value emission** —
a single AST walk that writes Value variants directly instead of
tape-then-walk-tape. **This is a cross-tranche refactor**, not a
wave-5-of-8 feature.

## IV. The duplication ledger

Audit 4 + Audit 3 + Audit 2 triangulate to one conclusion: the
pipeline computes the same structural facts multiple times and trusts
none of them.

- **FIRST-byte sets**: four implementations (`egraph/analysis`, 
  `sets/first_sets`, `shape_dispatch/unordered`, 
  `recognizers/disjoint_first`).
- **`EClassFacts`**: two (e-graph `GrammarAnalysis::make` vs
  `classify.rs::compute_eclass_facts` lines 530-870; classify.rs's own
  comment at line 98 acknowledges the re-implementation).
- **`is_operator_chain`**: three (`node_facts.rs:119-157`,
  `dta.rs::match_operator_chain_rule`, `shape_dispatch/pratt.rs:87-128`).
- **Wrap pattern**: four (`delim_scan::try_detect`, `shape_dispatch/
  object.rs`, `shape_dispatch/array.rs`, `balanced_wrap::BalancedWrapMiner`).
- **Alt-of-literal**: four (`keyword_stats.rs`, `key_dispatch.rs`, 
  `shape_dispatch/keyword.rs`, `shape_dispatch/alt_dispatch.rs`).

These are not coincidences. They reflect the surface-accumulation
pattern: each tranche added a pass that needed a structural fact,
and nobody plumbed the canonical source out. Every accumulation is a
divergence opportunity.

## V. Revised AY continuation

AY's remaining waves (W5, W6, W8, W7) were authored before these
audits. They target compile-time + parallel fork + FINAL declaration
under the assumption that W1-W4 would move throughput toward BEAT-sonic.
The audits show that assumption held only partially; AY is
**substrate-rich but consumer-sparse**. Ship forward with the existing
W5/W6/W8 plan would compound the dead-substrate debt.

**Revised continuation**: retire dead surfaces first, wire unified
cost model at existing consumers, ship the missing dev infrastructure,
close on documented evidence (not BEAT-sonic declaration).

| Wave | Scope | LOC Δ | Bench Δ |
|---|---|---|---|
| **WR1** | Retire 13 dead e-graph rules + handle.rs + phf.rs + fold structural_scan into BA if no consumer lands | **−1,100** | neutral (dead code has no runtime) |
| **WR2** | ShapeLattice replacement of 13-way if-cascade; fuse `shape_dispatch` into `mine_recognizers`; retire 4× FIRST-set duplication; plumb EClassFacts from e-graph write-back | **−1,450** | +compile time |
| **WR3** | Wire `CostWeights` at `byte_class.rs` + `payload/layout.rs`; NogoodStore→backtracker (−80 LOC delta after carve); Payload-layout CSP (admits CSS L4 Color at 16 B cap) | +100 net | +runtime (no more LargeAggregate branch for Color) |
| **WR4** | `scripts/prepare-profile-wave.sh` landing; W6 parse_that de-generic (−37 s cold build); `codegen-units=256` on ax-iter; bench-subset.sh runner; bootstrap idempotency CI gate | infra | **−55 % cold build; −80 s bench iter** |
| **WR5** | FINAL — bench matrix at retirement close, FINAL.md honest BEAT-sonic stance (not achieved single-tranche; substrate groundwork for BA fused parse+value emission), BA/BB/BC handoff contract | docs | — |

Total: **−2,450 LOC deletion; −55 % dev cycle; no new features.**

## VI. BEAT-sonic disposition

AY as plan-authored targeted BEAT-sonic by 15-40 % on twitter eager.
At W3c close bench: **3.63× slower than sonic on twitter eager**.

The plan's architectural thesis assumed the AU substrate restoration
(W1) + e-graph G3 wrap-elision (W2) + json-prototype shape (W3) +
SIMD unescape + Eisel-Lemire (W4) would compose to BEAT. Two of these
underperformed their projections structurally (G3 precondition absent;
pay_f64 separated a column that was already in pay_wide). The
remaining 4.27× multiplier to close the gap is not in one more wave —
it is in a different emission shape: **fused parse+value single-pass**,
modelled after sonic-rs's `Value::Number(f)` IS the node.

That is a BA theme. AY closes on substrate restoration + consumer
audit + retirement — **honest architectural progress**.

## VII. Handoff to BA / BB / BC

The revised AY delivers BA a substrate consisting of:

- Flat AoS `Vec<TapeRec>` tape (W1)
- Unified `CostWeights` + pluggable `CostModel<N>` (retained substrate,
  newly wired consumers via WR3)
- Named preservation end-to-end for non-pruned rules (W2.2 + W2.7)
- `<Grammar>Value` emission + per-shape `materialize_*` inline fns
  (W3b); cost of Value materialization measured honestly
- SIMD unescape inline at `parse_string` emission (W4.1; +6 % twitter)
- `pay_f64` direct-column (W4.2; substrate for SIMD reductions)
- Retired ~2,450 LOC of dead substrate (WR1 + WR2)

BA's natural next-tranche work:

1. **Fused parse+value emission** — single AST walk writes Value variants
   directly. This is the BEAT-sonic lever.
2. **Columnar SIMD reducer write-side wiring** — AW-IV.W5.1's 6.57×
   canada is a microbench of the substrate bbnf-lang already has.
3. **CSS L4 compile-time** — routed to BB rather than the direct
   performance tranche. `.bbnf-cache` surgical invalidation instead of
   SCHEMA bump full invalidate.
4. **Document-parallel fork** (from AY.W8, never dispatched) — ≥ 1 MB
   input multiplier.

## VIII. Operating directive for the revised AY execution

1. **No new features.** WR1-WR4 are deletions + consolidations +
   infrastructure. WR5 is documentation.
2. **Every change retires complexity.** No commit adds LOC without a
   matching reduction elsewhere, except WR4's infrastructure additions
   whose purpose is dev-cycle acceleration.
3. **Substrate-with-consumer is verified at commit.** Every retained
   substrate in WR3 gets a runtime consumer grepped + nm-verified +
   samply-attributed before the commit lands.
4. **Dev-cycle improvements land first** (WR4) so WR1-WR3 iterate
   faster.
5. **No gate-off commits.** Retirement commits DELETE; they do not
   feature-flag.

---

**Cross-audit citations**:
- E-graph firing: `AYW-egraph-cost-firing.md` § 2 + `crates/core/examples/egraph_fire_probe.rs`
- CSP under-wiring: `AYW-csp-scope.md` § 3 + § 4
- Duplication ledger: `AYW-structural-analysis-gap.md` § 4
- Self-time evidence: `AYW-retrospective-wiring.md` § aggregate;
  `.profiles/samply/post-AY-W1-fix/json_monolithic/twitter/top10-self-time.txt`
- Dev-cycle targets: `AYW-dev-expedite.md` § 4 + § 6
- AU-era erosions: `AYW-archaeology-optimization-arc.md` § 4
