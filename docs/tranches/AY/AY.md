# Tranche AY — Beat Sonic-rs: AU Substrate Restoration + Direct-to-Struct + Value API + Legacy Prune

AY is the AX continuation: performance + projection + closure. Successor to AX; predecessor to AZ (AZ carries replay/recovery/incremental tooling, `docs/tranches/AZ/AZ.md`). AX closed the interpreter at runtime (W0b), landed the shape-emission-authoritative tape (W0a), reverted hand-coded Value duplicates (W1r.0), proved grammar-derived view surface via canonical-serialization byte equality (W1r.2/3a/4a/5), audited the typed-accessor surface (W1r.6). Eight W1r sub-waves landed (`3429aaba`..`ab7c218d`), 13 parity + canonical harnesses green on master (247/1 ignored).

**AU archaeology revealed a ~4.5× regression from post-AU to current master.** Post-AU (`3b8b757`, Apr 15) shipped twitter at **1,967 MB/s = 0.615 bytes/cyc = 76% of sonic-rs**; citm at **94% of sonic-rs**. Current master (411eabfd): twitter at 437 MB/s = 0.137 bytes/cyc = 17% of sonic-rs. The regression chain: SoA 7-column write pivot (AV/AW-I) stacked atop compound-wrap tax introduced by AW-V.W6's gate over-admission. Both landed without perf gating; parity tests pass but the speed ceiling collapsed. `json-prototype` (AW-V.W2.1) demonstrated the underlying substrate is still capable of sonic parity (0.89-0.94× of sonic on 5 JSON fixtures); the regression is entirely emitter-shape + tape-substrate-layer.

**AY targets BEATING sonic-rs by 20-40% on twitter + canada + citm.** Not approaching, not matching — beating. The path is AU substrate restoration + e-graph G3 wrap-elision + json-prototype per-shape inline emission pattern + SIMD unescape + Eisel-Lemire direct-to-column. Each is load-bearing; each has archaeology as proof of feasibility.

Ten fresh audits (A1–A10) at master HEAD `411eabfd` establish the performance floor + absorption scope:

- **Parse throughput**: JSON 5.5-8.2× slower than sonic-rs (4.5× of that gap is AU regression, per AU FINAL bench table); CSS L4 **beats lightningcss 19-40%** at scale; Sheets/BBNF stable (+2-5% vs W0a.close).
- **Universal hot-path**: `Columns::push_structural` (23-43%) + `tape::finaliser::finalise` (12-27%) = **50-70% of every grammar's self-time**. Both symbols were introduced post-AU; both are direct regressions from AU's flat AoS + unified-arena substrate.
- **DTA at runtime**: fully retired (A8 cert). Zero DTA symbols in 4 bench binaries; zero DTA frames in samply profiles.
- **DTA at source**: ~2,300 LOC of kernel-dead legacy. AY.W0 prunes.
- **Structural alphabet**: mined but orphaned; walker retirement broke the reader chain. AY.W1 revives.
- **Named-type collapse**: `TypeDesc::Named(_)` drops before Rust emit. AY.W2 fixes.
- **AX W2-W15 absorption**: per A7 — 6 fold, 2 new, 3 RETIRE-as-obsolete, 1 defer AZ, 4 defer post-AZ.
- **AU regression triad**: (a) SoA 7-column write adds 7× bookkeeping per push vs AU's flat `Vec<TapeRec>`; (b) compound-wrap emission on every value/alt rule doubles the tape record count; (c) `push_structural` cross-crate call boundary not inlined despite `#[inline]` hint.

AY executes. No deferrals. No hand-coded Value duplicates. No substrate-without-consumer landings. No new grammar directives (invariant 4). Every wave ships substrate + consumer (invariant 2). All AY invariants are AX's 1-21 plus three new architectural gates: tape substrate inline (22), Named preservation end-to-end (23), Value API apples-to-apples work-matching (24).

## Architectural thesis

Six propositions:

1. **AU's substrate was near-optimal; restore it.** Post-AU shipped twitter at 1,967 MB/s = 76% of sonic-rs with flat `Vec<TapeRec>` + unified `push_leaf_with(kind, PayloadData)` + per-grammar push_fingerprint. The SoA 7-column pivot (AV/AW-I) was gated on a read-side microbench (1.94× canada f64-sum) without measuring write-side cost. Write-side cost dominated the parse path: 7 Vec pushes per record vs 1. The AoS sidecar we landed in W1.D is orthogonal — it's a read-side cache, not a write-side gain. AY.W1 reverts the write path to AoS `Vec<TapeRec>`, keeps the AoS sidecar as the read cache. This alone is a 2× recovery on twitter.

2. **Compound-wrap emission doubles the tape.** Current emitter wraps every `value` rule in a compound record: `mark_children` + N child pushes + `push_compound`. For JSON: 100K scalars → ~200K tape records (100K leaves + 100K wraps). Sonic-rs has no wrap — `Value::Number(f)` IS the node. E-graph G3 (Wrap-of-epsilon elision) + wrap-elision classification: single-variant Alt or scalar-projecting rules don't emit wrap compounds. Cuts twitter record count 50%.

3. **json-prototype is the speed-ceiling oracle; emit its shape.** AW-V.W2.1 (`4fdef7c3`, `0dcf9743`) hand-tuned JSON parser hit 0.89-0.94× of sonic across 5 fixtures. Five `#[inline(always)]` per-shape fns, no walker residue, inline SIMD + Eisel-Lemire. This shape is grammar-derivable: the shape emitter produces the equivalent layout when Wrap-of-epsilon elides + push_structural inlines + payload-data writes bypass arena. AY.W3 makes this the emission contract.

4. **Direct-to-struct must reach emit.** AU's invariant 2 (every `->` reaches tape) failed on Bugs 1 + 2. AY.W2 merges Named-preservation (A6) with e-graph G1-G9 cost-model repair (A7 absorbed) — they're the same work surface (both touch `crates/ir/src/egraph/` + `crates/ir/src/passes/metadata.rs`).

5. **Three-consumer surface, two perf benches + one correctness test.** NodeView is lazy, serialize_compact is text, `to_value` is eager. The API is ONE substrate (tape) with three consumer patterns. Benches measure **lazy-get-by-path** + **eager-materialized** (matched-work vs sonic-rs `get_by_path` + `from_str::<Value>` respectively). Canonical-serialize is the round-trip correctness gate (already landed W1r.2), NOT a perf lane.

6. **Legacy prune is precondition housekeeping.** ~2,300 LOC of DTA kernel-dead + stale wire-contract tests + dead GrammarProfile fields pollute both compile-time and intellectual scope. Retire in W0 before any substrate work.

## Invariants

All AX invariants 1-21 carry forward verbatim. AY adds three:

22. **Tape substrate inline + AoS primary.** Write path is flat `Vec<TapeRec>` — 16 bytes per record, one `Vec::push` per structural record. `push_structural` (if retained for back-compat) is `#[inline(always)]` + LTO-verified inlined at every emit site. `push_leaf_with(kind, PayloadData)` is the sole payload-bearing leaf entry point (restoration of AU.6.7). Finalise fusion: sib_skip + child_off + span_hi back-patches at `close_compound` rather than a post-pass sweep. Wave close verifies via `nm` (zero cross-crate tape push exports) + samply self-time shifting from `tape` crate into per-rule `parse_*` functions.

23. **Named preservation end-to-end + wrap-compound elision.** Every grammar-declared `-> input : <Name>` annotation (non-scalar per scalar-name table) reaches the Rust tape emitter as `TypeDesc::Named(sid)`. `emit_direct_to_struct_projection` admits the rule and emits the runtime shim + aggregate payload pathway. Additionally: e-graph G3 (Wrap-of-epsilon elision) + G1-G2 (Alt/Repeat-of-single) + G4 (adjacent-literal concat) fire as IR-canonicalisation passes before emit; rules whose body collapses to a single scalar-projecting form don't emit a wrap compound. Enforced by `named_type_preservation.rs` + `wrap_compound_elision.rs` wire-contract tests.

24. **Value API apples-to-apples.** Bench comparisons vs external comparators are work-matched along one of three surfaces: (a) canonical-serialize text equality (W1r.2 landed; correctness test, not perf bench), (b) lazy-get-by-path (bbnf `Parsed::get<T>(path)` vs sonic-rs `get_by_path`), (c) eager-to-eager (bbnf `parsed.to_value::<T>()` vs sonic-rs `from_str::<Value>`). Mixed-work comparisons forbidden in reported headline ratios.

Invariant 2 (substrate-with-consumer) discharges retroactively for `structural_alphabet` mining that shipped without reader since W0b — AY.W1 activation closes the cycle. Invariant 14 (gate-predicate symmetry) discharges retroactively for 5 stale wire-contract tests whose predicates retired in W0a.2.j / fields carved in W0b.A.

## Operational posture

1. **Bench-checkpoint every wave** with bytes/cyc attribution, not just ns/iter. Saved to `docs/benchmarks/post-AY-W<N>.json`. Regression ≥ 5% triggers re-plan per AX invariant 10. Additionally: BEAT-sonic target is a hard gate at W7 close.

2. **Fresh profile-prepare at mid-tranche** — re-run `scripts/prepare-profile-wave.sh` at W3 close for W4+ agents. Prevents stale-profile recurrence.

3. **Wire-contract tests for every new invariant.** Invariant 22 → `post-AY-W1-close-nm.txt` + samply shift. Invariant 23 → `named_type_preservation.rs` + `wrap_compound_elision.rs`. Invariant 24 → `value_api_apples_to_apples.rs`.

4. **Scope-reveal Absorb** per SPEC. Wave scope-reveal reopens as sub-wave in place; no letter pivot mid-execution. Exception: if any wave's scope-reveal threatens the BEAT-sonic target, orchestrator flags to user immediately.

5. **No grammar DSL additions.** Invariant 4 preserved. Every change is IR pipeline / tape substrate / codegen / bench-harness / IR-consumer wiring.

6. **AU archaeology drives every lever.** AU FINAL bench numbers are the restoration target for Phase 1 (W1 + W2); json-prototype numbers are the ceiling target for Phase 2 (W3 + W4). Every wave close cites AU delta.

## Wave summary

**W0-W4 executed as originally planned** (landed per `PROGRESS.md`).
At W3c close: twitter eager `bbnf_value / sonic_value = 3.63×` (target
was ≤ 0.85× for BEAT-sonic declaration).

A six-agent retrospective audit (`audit/AYW-SYNTHESIS.md`) at master
HEAD `f352bcdc` surfaced:

- 13 of 14 e-graph rules fire zero times on production grammars
  (normalizer subsumes their match surface before e-graph runs)
- W3a `handle.rs` (142 LOC) + W4.3 `phf.rs` + W1.3 `structural_scan`
  all zero runtime consumers (invariant-2 violations)
- Classifier 8,114 LOC with 4× duplicated FIRST-sets, 3× `is_operator_
  chain`, 4× wrap-pattern, 4× Alt-of-literal detection
- `egraph::CostWeights` unified at substrate but under-wired at two
  consumers (`byte_class.rs`, `payload/layout.rs`)
- Twitter parse self-time: 55 % `<JsonParser>::parse` (LTO-inlined
  body), 24 % `parse_object`, 12 % `parse_wrap`, ≤ 1 % tape substrate
- Cold dev cycle 12-15 min; 45-90 s achievable per `AYW-dev-expedite.md`

**Revised continuation**: retire dead surfaces first, wire unified
cost model at existing consumers, ship the missing dev infrastructure,
close on documented evidence. The BEAT-sonic multiplier (4.27× from
W3c) lives in **fused parse+value single-pass emission** — a cross-
tranche theme AZ owns. AY closes honestly on substrate groundwork.

**Waves W5 / W6 / W8 are superseded** by WR1-WR4. The deferred scopes
(CSS L4 @import split, parallel fork) move to AZ with the fused
emission workstream.

### Executed waves (W0-W4)

| Wave | Outcome | Artefact |
|------|---------|----------|
| **W0** | ✅ Legacy prune + AX.FINAL + housekeeping (−2,799 LOC tests + −470 dta.rs + −78 shape_dict.rs + −2,098 profile files + 37 worktree orphans) | `waves/W0.md` + `PROGRESS.md` |
| **W1** | ✅ AU AoS substrate revert + finalise stack-buffer + Pratt Option C + structural-scan substrate. W1-fix absorbed twitter regression (420 → 688 MB/s). Closes 2× of the AU regression (0.137 → 0.215 bytes/cyc; SOFT-MISS on the 0.45 floor) | `waves/W1.md` + `audit/AYW1-twitter-regression-diag.md` |
| **W2** | ✅ Named preservation (Fix A — colorFn/colorMix precedence wrap; Fix B reachability deferred due to Wrap-emitter Alt-priority regression) + defensive guards + G1-G4 egraph rules (shipped; 13/14 dead on production grammars per audit) + wrap-compound elision (9 % record-count reduction on 3 BBNF rules; G3 precondition absent on JSON) + EBNF Minus codegen fix + wire-contract tests | `waves/W2.md` + `audit/AYW2-named-collapse-probe.md` |
| **W3** | ✅ Handle + Path substrate (W3a; consumer-less per audit) + `<Grammar>Value` enum emission + per-shape `materialize_*` inline fns (W3b; 8 grammars, 48 fns) + 12-entry Value bench lanes + round-trip parity + BEAT-sonic gate (**3.63× — MISSED**) | `waves/W3.md` + `docs/benchmarks/post-AY-W3-value.json` |
| **W4** | ✅ SIMD unescape inline at `parse_string` emission (+5.95 % twitter) + `pay_f64` direct-column substrate (bench-neutral on canada; spec narrative incorrect about pre-W4.2 arena round-trip) + regex specialisation scaffolds (byte_class + phf + last_byte_set + DFA hoist; CSS tailwind `__regex_scan` self-time regressed +3.18 pp — SHIPPED soft-miss) + structural-scan consumer on Sheets only | `waves/W4.md` + `docs/benchmarks/post-AY-W4-close.json` |

### Retirement / consolidation continuation (WR1-WR5)

**No new features.** Every change retires complexity. Substrate-with-
consumer verified per commit (grep + nm + samply).

| Wave | Scope | LOC Δ | Primary artefact |
|------|-------|------:|------------------|
| **WR1** | Retire 13 dead e-graph rules (or fold G1-G4 + CommonSuffixFactor into the structural normalizer where they'd actually fire); delete W3a `handle.rs` (142 LOC, zero consumers); delete W4.3 `phf.rs` (zero callers); assess W1.3 `structural_scan` retire-or-consumer | **−1,100** | `waves/WR1.md` |
| **WR2** | Replace classifier's 13-way if-cascade with `ShapeLattice` + partial order; fuse `shape_dispatch` into unified `mine_recognizers` walk (cached SCC topological order); retire 4× FIRST-set duplication; plumb `EClassFacts` from e-graph write-back (delete `classify.rs::compute_eclass_facts` 350 LOC duplicate) | **−1,450** | `waves/WR2.md` |
| **WR3** | Wire `egraph::CostWeights` at `byte_class.rs` + `payload/layout.rs` (unified-cost consumers that bypass it today); wire `NogoodStore` (167 LOC on-shelf) into backtracker; rebuild payload-layout planner on CSP (admits CSS L4 Color at 16 B hot-path; closes A6 chronic) | **+100** net | `waves/WR3.md` |
| **WR4** | Ship `scripts/prepare-profile-wave.sh` (PROFILING.md contract broken); apply W6 parse_that de-generic (~−37 s cold build); `codegen-units=256` on `ax-iter`; `scripts/bench-subset.sh` (−80-90 s per iteration); split `serialize_roundtrip.rs` per-grammar; bootstrap idempotency CI gate | infra | `waves/WR4.md` |
| **WR5** | **FINAL** — bench matrix at retirement close; `FINAL.md` honest BEAT-sonic stance (not achieved single-tranche; substrate groundwork for AZ fused parse+value emission); AZ handoff contract | docs | `waves/WR5.md` |

Total: **−2,450 LOC deletion; −55 % cold dev cycle; no new features.**

## AY → AZ handoff contract

AY closes on **substrate restoration + honest consumer audit + dead-
surface retirement**, not on BEAT-sonic declaration. The BEAT-sonic
multiplier lives in AZ's fused parse+value emission workstream.

Seven conditions must verify clean before AZ opens:

1. `cargo test --workspace` green (1490+ passed / 0 failed, within
   documented Category-A ignore budget).
2. `post-AY.json` bench matrix captured over 19 parse entries + 12
   Value-API-lane entries; ratios documented with honest disposition
   (no BEAT-sonic declaration).
3. `named_type_preservation.rs` + `wrap_compound_elision.rs` +
   `value_api_apples_to_apples.rs` all green; BEAT-sonic sanity gate
   documented as ignored with explicit ratio in `docs/benchmarks/
   post-AY-W3-value.json`.
4. `nm` on all 4 prebuilt bench binaries: zero `push_structural` /
   `finalise::finalise` cross-crate exports (invariant 22 verified at
   W1 close).
5. **Dead substrate retired**: `handle.rs`, `phf.rs`, and ≥ 12 of 14
   dead e-graph rules either folded into the normalizer or deleted
   (WR1). Classifier LOC reduced by ≥ 1,200 (WR2). Unified
   `CostWeights` consumed at `byte_class.rs` + `payload/layout.rs`
   (WR3).
6. `scripts/prepare-profile-wave.sh` present + working (the
   operational contract in `docs/instructions/PROFILING.md` is
   satisfied).
7. Bootstrap regen cycle-1 = cycle-2 byte-identical at every WR close.

## Defensible floor (revised)

Per the post-audit `AYW-SYNTHESIS.md` honest accounting, the floor is
**five items**:

1. **W0 legacy prune** — landed (~2,799 LOC tests + ~470 dta.rs + 78
   shape_dict.rs + 2,098 profile files + 37 worktree orphans).
2. **W1 AoS revert + finalise stack-buffer + Pratt Option C** — landed.
   Closes half of AU regression (0.137 → 0.215 bytes/cyc on twitter).
3. **W2 + W3b Named preservation + `<Grammar>Value` + `materialize_*`
   inline fns** — landed, honestly (Value lane measured at 3.63× sonic,
   not the projected ≤ 0.85×).
4. **WR1 + WR2 + WR3 retirement + unification** — the substrate-truth
   reconciliation. Without this, every subsequent tranche inherits the
   dead-substrate debt AY surfaced.
5. **WR5 FINAL** — bench matrix + `FINAL.md` + AZ handoff + CI gate
   activation on retained wire-contract tests (`named_type_preservation`,
   `wrap_compound_elision`, `value_api_apples_to_apples`).

The path to BEAT-sonic is an AZ theme: **fused parse+value single-pass
emission**. Items 1-4 deliver AZ a clean substrate; item 5 declares
the honest stance.

## Post-tranche review candidates

Decision at AY.W7 close, not mid-wave:

- **CSS `calc()` semantic evaluator** (W1r.3a scope-reveal) — bootstrap/tailwind byte-parity against lightningcss requires calc() arithmetic canonicalization. Dedicated workstream OR drop to AZ tooling scope.
- **Shape-dispatch detector retirement** (AX.W12) — folds into AY.W2 e-graph work; detectors retire as e-graph G5-G9 subsume their logic.
- **Multi-visitor pairs + multi-key SIMD compare** (AX.W14) — small LOC, small gain; bench-gated at W7 close.

## Indefatigability

When AY closes:

- **BEAT sonic-rs by 15-40% on twitter + citm + canada eager-materialised bench.** First Rust JSON parser to beat sonic-rs at its own benchmark.
- Tape hot-path is flat AoS `Vec<TapeRec>` write + lazy AoS sidecar read; no cross-crate call boundary on emit.
- Structural alphabet + CTNS reactivated; dense-alphabet pre-scan consumed at `parse()` entry.
- Pratt Option C within-compound inline; reducer-compound tree preserved (parity unbroken).
- Direct-to-struct projection fires for every grammar-declared `-> input : <Name>`; `PROJECTION_DIRECT_TO_STRUCT` const has ≥ 4 entries.
- E-graph G1-G9 universal + per-shape rewrites active; wrap-compound emission eliminated; shape-dispatch detectors retired (~1,676 → ~150 LOC classifier cleanup).
- Value API apples-to-apples bench matrix populated across canonical-serialize (correctness) + lazy-get + eager-materialized (2 perf lanes).
- CSS L4 compile-time + cache-size reduction on top of W1r.3a's 69% drop.
- Legacy pruning: ~2,300 LOC deleted (dta.rs kernel-dead 458, 5 stale tests 1,390, dead GrammarProfile 150, shape_dict 79, 2 emitter tests 220).
- Document-parallel fork delivers amortisation multiplier on ≥ 1 MB inputs — combined with single-thread beat, total twitter throughput ≥ 5-7 GB/s on multi-core (surpasses simdjson's multi-core record).
- Five stale wire-contract tests retired; ebnf_prettify recognizer green; `cargo test --workspace` clean.
- AX FINAL.md written; AZ handoff artefacts ready.
- No hand-coded Value duplicates; no third-party comparator bridges; no substrate-without-consumer landings; no placeholder variants.
- Invariants 22-24 + AX 1-21 all gated by per-wave wire-contract tests and samply attribution.
