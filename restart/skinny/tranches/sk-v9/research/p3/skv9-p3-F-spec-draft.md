# SK-V9 P3-F: SPEC Draft — Recovery + Behavior Wave Plan

Pass: S-P3 Synthesis-Plan. Cycle: V9.
Date: 2026-05-18.
Scope: Draft the next SK-V9 SPEC — §0 close condition + goalset, §0.x
telemetry schema, §1 non-negotiables, §2 wave manifest, §3+ per-wave
sections — folding the S-P1 converged profile and the six S-P2
converged interventions into a wave-sequenced, falsifiability-gated
contract.
Output: this file (a DRAFT; the orchestrator promotes it to
`sk-v9/SPEC.md` after S-P3 CHALLENGE converges).
Pass Alpha goalset: SK-V9 §0 close condition carried from the current
SPEC plus the S-P1/S-P2 evidence; the four uncloseable rows, the
structural-rediscovery hot leaf, and the Apache/CITM typed-GO lift.
Candidate pool: `research/p2/` post-CHALLENGE survivors (P2-A..F).

Integration note: sibling P3 artefacts P3-A (shortlist), P3-B
(sequencing), P3-C (gates), P3-D (telemetry), P3-E (pre-blocked
ledger) were not present at `research/p3/` when this draft was
authored. This draft is composed directly from the S-P2 converged
evidence (`HARDENING-S-P2-CONVERGED.md` plus the six P2 reports). The
integration points where a sibling artefact, when it lands, supersedes
a drafted section are marked `[INTEGRATE P3-x]`.

---

# SK-V9 SPEC — Recovery + Behavior Wave Plan

Date: 2026-05-18.

Status: post-G-Alpha, post-W0 telemetry-lock, post-S-P1-rerun, and
post-S-P2-converged. W0 closed `G-W0-TELEMETRY-LOCK` under
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`. The S-P1 rerun converged
six-of-six lenses against fresh SK-V9-open evidence after the P1-D
PMU/cycles blocker was cleared by `xctrace` plus an accepted Xcode
license; the real PMU table lives at `/tmp/skv9-xctrace-v3/pmu_rows.tsv`.
S-P2 Research converged six-of-six lenses and handed six interventions
to S-P3. This SPEC is the wave plan that lands them.

Authority:

- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v9/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md`
- `restart/skinny/tranches/sk-v9/research/p3/` — P3-A..F cohort.
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Dispatch lock:

- G-Alpha is closed.
- W0 telemetry-lock is closed: `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
- The S-P1 rerun converged; `G-S-P1-RERUN-CONVERGED` PASS is recorded
  in `HARDENING-S-P1-CONVERGED.md`.
- S-P2 Research converged; the six interventions are dispatchable in
  the dependency order of Section 2.
- No behavior wave dispatches until its own entry gate passes. A wave
  is dispatch authority only after `G-BEHAVIOR-RELEASE` and the
  per-wave entry gate of Section 3+ both pass.

## Section 0 — Close Condition And Goalset

### Section 0.1 — SK-V9 Close Condition

SK-V9 closes only when all of these are true:

1. W0 produced and `gate-json` consumed a coherent `SK-V9-open`
   telemetry manifest — recorded closed.
2. The post-W0 S-P1 rerun converged on SK-V9-open evidence and named
   fresh PMU/cycles rows for every behavior candidate — recorded
   `G-S-P1-RERUN-CONVERGED` PASS.
3. `G-BEHAVIOR-RELEASE` passed: every behavior wave has either
   admitted by its named row gate or rejected with REDRESS measurement.
4. The W2 retained-grammar proof is accepted under proof-first
   CHALLENGE and the union reopen (W3) is unblocked by it.
5. Strict admission remains strict-vs-strict on matching output planes
   only; no row admits on stale, permissive, lossy, absent, historical,
   sidecar-only, or view-boundary evidence.
6. The four P1-named uncloseable rows (`unicode_escapes`,
   `unicode_mixed`, `y_string_unicode`, `gsoc-2018`) either admit by
   the W5 conditional same-wave rule or are recorded NEAR-FAIL / FAIL
   with the honest projection in REDRESS.
7. The W10b six-row regression block (`canada`, `citm_catalog`,
   `instruments`, `marine_ik`, `mesh`, `numbers`) holds its maintain
   floor at every wave that touches the parse loop.
8. `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SPEC.md`,
   `DISPATCH-PROMPT.md`, and `HANDOFF.md` agree at close.

### Section 0.2 — Goalset (carried from Pass Alpha + S-P1/S-P2 evidence)

The convergent S-P1 verdict (`HARDENING-S-P1-CONVERGED.md` §"Load-bearing
diagnoses"): bbnf's parse-plane losses are **substrate-bound, not
kernel-bound**. Four diagnoses bind the goalset:

| # | S-P1 diagnosis | Goalset target | Owning wave |
|---|---|---|---|
| 1 | `scan_structurals` is 0.00% self-time on every row — the SIMD stage-1 index is discarded; the parser re-discovers structural bytes scalar. | `consume_structural` ≤ 5% self-time on `twitter`, `apache_builds`; `JsonNodeKind::at_cursor` ≤ 1%. | W3 |
| 2 | String-scanner pair (`match_tiny_plain_string` + `match_string_at_quote`) reaches 47-67% self-time on dense-key losses. | 32-byte string-block widening lifts the string-dense losses. | W4 |
| 3 | Unicode-escape codec (`read_hex_unit_scalar` + `hex_nibble`) = 38-44% on `y_string_unicode`. | `escape_codec_hex_unit` SIMD primitive paired with the scanner widening. | W5 |
| 4 | OLS fit `ns_per_byte ≈ 1.079·(q/B) + 0.184·(n/B) + 0.051`, R²=0.371. Four LOSS rows exceed 130-460% of the per-byte budget — delimiter-only intervention is insufficient. | The four uncloseable rows need the codec AND the scanner widening; neither closes them alone. | W4 + W5 |

The cheapest GO-count lift is substrate-independent: Apache/CITM
measured typed-row admission (P2-C) is a mechanical baseline-whitelist
expansion and lands first among the behavior waves.

### Section 0.3 — Opening Baseline And Row Families

The opening benchmark authority is the W0-rendered 38-row JSON report
under `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.

| Family | Row count | SK-V9-open posture |
|---|---:|---|
| `parse_only` | 17 | 17 `S / NO-GO`; baseline rows only; no strict SOTA admission until a wave lifts a named row. |
| `direct_to_struct` | 17 | 3 `A / GO`, 14 `N-direct / NO-GO`; direct digest is a guard plane, not typed product proof. |
| `real_typed_struct` | 4 | 4 `A / GO`; Apache/CITM/Canada measured typed rows are not present until W1 admits them. |

All current main rows remain `Strictness=deferred`. Native Rust
comparators are same-run; C++ sidecars are historical or absent.

### Section 0.4 — Required Telemetry Schema

`[INTEGRATE P3-D]` — when `skv9-p3-D-telemetry-schema.md` lands, its
column binding and `gate-json` rejection rules supersede this section.
Until then this carries forward the W0 schema.

W0 bound the telemetry schema; every behavior wave's added rows must
populate every required field or `gate-json` fails the wave closed.
The required fields are the W0 set:

```text
row_id            grammar_id        domain            corpus
workload          outcome_id        verdict           strictness
output_plane      track1_mbps       track2_mbps       comparator_id
comparator_plane  comparator_strictness                comparator_freshness
measured_validation_path             profile_artifact  sample_cost
sample_count      build_flags       host_triple       feature_mask
costfacts_rule_id costfacts_chosen_shape               costfacts_rejected_alternative_ids
redress_entry     wave_id           run_id            sidecar_freshness
sk_v9_open_delta  substrate_surface structural_projection_status
substrate_cardinality                same_wave_consumer_class
track2_independence_status           diagnostic_nonproducer_status
```

SK-V9 behavior waves add three required fields, gate-consumed in the
same wave that emits them:

```text
checkasm_parity_status   — PASS | N/A; every wave landing a SIMD/ASM
                           primitive must carry a green differential
                           test or the row rejects.
union_class_column_status — present | absent | N/A; W3 must report the
                           class column is co-indexed and SIMD-filled.
codec_admission_basis    — strict | conditional-same-wave |
                           no-regression | rejected; W5 rows must
                           disclose which §5.x rule admitted them.
```

Producer-only telemetry rejects. A field emitted but not consumed by
`gate-json` in its wave is a producer-only artefact and fails the wave.

### Section 0.5 — Comparator Classes

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | sonic-rs strict, serde_json on matching output plane | May support strict admission only if the comparator plane matches the row output plane, `comparator_strictness=strict`, the comparator is a same-run native strict anchor admitted by id, and UTF-8/control/escape validation occurs inside the measured row. |
| Same-run flaw probe | sonic-rs lossy, permissive rows | Planning only; never strict admission. |
| Sidecar planning signal | simdjson, yyjson, RapidJSON; asmjson has no aarch64 backend (P2-F) | Planning only; never strict admission and never an anchored sidecar on this host. |

## Section 1 — Non-Negotiables

- No new directive.
- No new BIR variant.
- No new `BackendShape` variant.
- No `UnionTape`. The W3 union event-model is a co-indexed class
  column on the existing offset tape, not a new tape type — Lock 1
  substrate cardinality stays at one.
- No new substrate surface; no public substrate API.
- No parser-owned structural cursor or parser-owned fact slot. The W3
  SIMD index is a transient producer consumed by move.
- No parallel or sidecar substrate.
- No JSON policy in generic crates. Every generic-crate edit carries a
  non-JSON proof (Section 2.1).
- No strict admission except strict-vs-strict on a matching output
  plane.
- No stale, permissive, lossy, absent, historical, sidecar-only, or
  view-boundary evidence as strict admission.
- No Apache/CITM/Canada measured typed row admitted from source/product
  parity alone — only fresh same-run run-id/metadata measured rows.
- No Canada typed shortcut through length, digest, schema, field-count,
  coordinate-count, or partial-fixture evidence.
- No direct digest row relabeled as typed product proof; scalar-parent
  folding stays blocked by REDRESS 93.
- No structural-scan-only, masking probe, PMU, or Criterion slope
  artifact used as a producer for Track 1, Track 2, typed product,
  direct product, or strict admission. PMU/cycles remain diagnostic.
- No primitive ships without a scalar reference, a checkasm
  differential test, and a same-wave hot-path consumer.
- No behavior source change without a same-wave consumer and a
  measured row gate.
- No wave closes on a future-phase promise. "Wired" or "integrated"
  without a bench-row threshold is a paper-close.
- Research, plan, CHALLENGE when first-of-class, redress, and close
  remain distinct commits (`SKINNY-TRIUMVIRATE.md` §9).

## Section 2 — Wave Manifest

`[INTEGRATE P3-B]` — when `skv9-p3-B-wave-sequencing.md` lands, its
per-wave entry gates and conditional-dispatch status supersede this
manifest. The dependency order below is drawn from
`HARDENING-S-P2-CONVERGED.md` §"The convergent picture": P2-B proof →
P2-A union → P2-D consumers; P2-E codec independent but conditional;
P2-C fully independent.

| Wave | Section | Name | S-P2 source | Dispatch status | Source LOC budget | Hard cap |
|---|---|---|---|---|---:|---:|
| W0 | Section 3 | SK-V9-open Telemetry-Lock Recovery | — | Closed | telemetry/gate/report only | <=90 min |
| W1 | Section 4 | Apache/CITM Measured Typed-Row Admission | P2-C | Dispatchable — independent, no substrate dependency | <=300 hand | <=90 min |
| W2 | Section 5 | Retained Class/Event Grammar Proof | P2-B | Conditional on W0 close + proof-first CHALLENGE | <=425 hand, 0 generated | <=90 min |
| W3 | Section 6 | Union Event-Model — Class-Column Substrate | P2-A | Conditional on W2 proof acceptance | <=265 hand + <=120 regen | <=90 min |
| W4 | Section 7 | aarch64 ASM Consumers — String-Block Widening | P2-D | Conditional on W3 close (the union substrate is the consumer base) | <=300 hand + tests | <=90 min |
| W5 | Section 8 | Unicode-Escape Codec — Conditional Admission | P2-E + P2-D §3 | Conditional on W4 close (the codec is paired with the scanner widening) | <=600 hand + <=120 regen | <=90 min |
| W6 | Section 9 | Close And Alpha Feedback | — | Conditional on W1-W5 dispositions | docs only | <=90 min |

LOC budgets are conjunctive with the 90-minute cap. They count
hand-edited source, tests, gate/report code, and hand-written doc
edits the wave names. Generated outputs do not consume the source LOC
budget, but every generated file is named, diff-audited, and included
in the revert slice. A wave plan exceeding either bound splits before
dispatch or returns REVISE.

Phase caps per `SKINNY-TRIUMVIRATE.md` §7: Research 30 min × ≤6
agents; Plan 30 min; CHALLENGE 90 min when first-of-class or
substrate-touching; Redress 75 min (60 impl + 15 measure).

### Section 2.1 — Generality And Lock 14 Gate

Every wave carries this exit gate; the checks tighten when generic
crates are edited:

- Public API scan: no new public JSON-named API in generic crates
  (`bbnf-simd`, `parse-that-regex`, `codegen` outside per-grammar
  template files, `runtime` outside `grammars/json/`).
- Grammar branch scan: no generic branch selects behavior by JSON
  grammar name, corpus name, object/array role, field name, or
  punctuation meaning.
- Primitive/table scan: no generic primitive, SIMD table, or
  classifier embeds JSON structural policy unless it is generated
  byte-set data plus opaque class ordinals with a scalar reference
  and a same-wave consumer. The W3 class column stores opaque class
  ordinals; the structural-alphabet `class_table`
  (`bbnf-simd/src/lib.rs:41`) is generated data.
- Non-JSON proof: every generic-crate edit (W3 codegen template, W4
  `string_block.rs`, W5 `escape_codec_hex_unit`) carries a CSS L4 /
  Sheets / BBNF-self proof — a named no-op dry run, focused test, or
  unchanged-output audit. P2-A names the CSS L4 / Sheets / BBNF-self
  union instances; P2-B names the Sheets `EventGrammar` witness; P2-E
  names the five const-generic codec bindings (JSON-4, CSS L4
  variable, JS variable, TOML-4, TOML-8).

## Section 3 — W0 SK-V9-open Telemetry-Lock Recovery

Status: closed. Close artifact:
`restart/skinny/tranches/sk-v9/research/skv9-W0-close.md`.
Run id: `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.

W0 made the opening telemetry self-consistent as `SK-V9-open`,
`gate-json`-consumed, and froze all behavior surfaces. `G-W0-TELEMETRY-LOCK`
PASSED: the manifest carries exactly the 38 main row identities, one
uniform run id, no Apache/CITM/Canada measured typed rows, behavior
freeze paths unchanged, and structural-scan/masking/PMU/cycles
diagnostic non-producers. W0 is not redispatched unless a later
CHALLENGE finds a concrete telemetry-lock defect and names a
revert/redress slice.

## Section 4 — W1 Apache/CITM Measured Typed-Row Admission

S-P2 source: `skv9-p2-C-apache-citm-admission.md`. Triumvirate shape:
research (archived P2-C cohort) → plan → redress; CHALLENGE optional —
this is a mechanical baseline-whitelist expansion, not first-of-class.

Objective: REDRESS 91 admitted Apache/CITM source/product parity but
not measured rows because the `SK_V8_OPEN_BASELINE` whitelist was not
expanded with W2's admission (P2-C §1). W1 captures a fresh same-run
Criterion row for Apache and CITM, flips the gate test assertions, and
promotes two `real_typed_struct A / GO` rows.

Owner paths (the seven P2-C paths; any other source path returns
REVISE before editing):

| Path | Allowed W1 use |
|---|---|
| `skinny/crates/bbnf-bench/src/gate.rs` (`gate.rs:1820-1831`) | Flip the `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures` Apache/CITM assertions from `!w0_real_typed_metadata_expected(...)` to `w0_real_typed_metadata_expected(...)`. |
| `skinny/crates/bbnf-bench/src/` baseline whitelist | Expand `SK_V8_OPEN_BASELINE` (the V9 equivalent) with the Apache/CITM admission. |
| `skinny/RESULTS.md` | Promoted row block — two `real_typed_struct A / GO` rows + two schema-v3 telemetry rows; run-id refresh across the file. |
| `skinny/REDRESS.md` | New entry recording the promotion, fresh run-id, the no-regression guard on the four existing typed GO rows, the Track 2/oracle independence claim, per-row throughput. |
| `restart/skinny/tranches/sk-v9/HANDOFF.md` | Move the candidate from "may admit" to "admitted under SK-V9 W1". |
| `restart/locks/LOCKS.md` (Lock 14) | Add `sk-v9-real-typed-w1` parent-diff allowance scoped to the seven owner paths. |
| `restart/skinny/tranches/sk-v9/research/` | W1 research/plan artefacts. |

Entry gate: W0 closed; `G-S-P1-RERUN-CONVERGED` PASS; the W1 plan
names the fresh capture method and the per-row Mbps falsifiability
threshold.

Exit gate `G-W1-TYPED-ADMISSION` passes only if:

1. A fresh same-run Criterion capture is produced across 21+2 rows ×
   the typed Criterion ids with `RUSTFLAGS="-C target-cpu=native"`.
2. Apache `track1_real_typed_struct` ≥ `ceil(sonic_rs strict / 1.10)`
   — the +/- 9.1% strict slack the four existing typed GO rows hold
   (twitter +0.7%, update_center -4.5%, mesh +4.6%, marine_ik +25.2%).
3. CITM `track1_real_typed_struct` ≥ `ceil(sonic_rs strict / 1.10)` at
   the same slack.
4. The four existing `real_typed_struct A / GO` rows maintain GO with
   no regression beyond noise.
5. The promoted rows carry independent Track 2 or oracle evidence; no
   row admits on Track 1 ≡ Track 2.
6. `cargo xtask gate-json --advisory --check-results` succeeds after
   the RESULTS promotion.
7. Section 2.1 generality scan passes — no JSON policy enters a generic
   crate.

Revert protocol: if the gate test refuses to compile or the assertion
flip exposes baseline/fixture drift, revert both assertions to
`!expected`; if `gate-json --check-results` fails after promotion,
revert `RESULTS.md` to the pre-promotion run-id snapshot; if Lock 14
`lock14_baseline` fails, revert the LOCKS.md allowance and route the
wave through a Lock 14 amendment. Record REDRESS; do not close by prose.

Pre-blocked routes: REDRESS 91 source/product-parity overclaim — W1
must cite REDRESS 91, name the fresh run-id, and not relabel
source-eligible rows as measured. `[INTEGRATE P3-E]`.

## Section 5 — W2 Retained Class/Event Grammar Proof

S-P2 source: `skv9-p2-B-retained-grammar-proof.md`. Triumvirate shape:
research → plan → **mandatory CHALLENGE** (first-of-class proof
surface) → redress.

Objective: REDRESS 92 rejected the SK-V8 W3 union before source
redress because the storage-only-swap framing broke the retained-view
contract. W2 lands a *compile-time* proof — an `EventGrammar` trait, a
`ValueRef<G>` cursor, and JSON + Sheets witnesses behind
`cfg(feature = "proof")`. Proof-only depth: **zero `RESULTS.md` row
movement**, zero generated output, zero production consumer. The proof
exists to unblock the W3 union reopen.

Owner paths (P2-B §1.2 / §6.1, ~425 hand LOC, 0 generated):

| Path | Role |
|---|---|
| `runtime/src/proof/event_grammar.rs` (new) | `EventGrammar` trait definition. |
| `runtime/src/proof/value_ref.rs` (new) | `ValueRef<G>` cursor — borrows the retained view; borrows nothing the parser owns. |
| `runtime/src/proof/json_witness.rs` (new) | JSON `impl EventGrammar`. |
| `runtime/src/proof/sheets_witness.rs` (new) | Sheets `impl EventGrammar` — the Lock 14 non-JSON exerciser. |
| `runtime/src/proof/any_grammar.rs` (new) | `AnyGrammar` empty-alphabet default instance. |
| `restart/skinny/tranches/sk-v9/research/` | W2 research/plan artefacts. |

All proof files are `cfg(feature = "proof")`; the default build is
byte-identical. No file under `grammars/json/` or `codegen/` is
touched.

Entry gate: W0 closed; `G-S-P1-RERUN-CONVERGED` PASS; the W2 plan
states the proof is compile-time and names the `cfg(feature = "proof")`
isolation.

Exit gate `G-W2-RETAINED-PROOF` passes only if:

1. `cargo build -p runtime --features proof` and
   `cargo test -p runtime --features proof` succeed.
2. The default build `cargo build -p runtime` is byte-identical to the
   pre-W2 build — the proof is fully behind `cfg`.
3. `ValueRef<G>` borrows only the retained view; a Lock 1 audit
   confirms it owns no parser cursor or fact slot (P2-B §2.2).
4. The Sheets witness compiles and the Lock 14 audit commands (P2-B
   §3.3) report no JSON-named symbol in a generic crate.
5. `skinny/RESULTS.md` is byte-identical — the proof moved zero rows.
6. The proof-first CHALLENGE accepted the owner paths, the LOC budget,
   the revert slice, and the no-production-consumer posture.

Revert protocol: if the proof cannot be expressed as a compile-time
contract, or if `ValueRef` cannot borrow without a parser-owned slot,
revert the five proof files and record REDRESS — the W3 union reopen
stays blocked. The proof is the gate on W3; a failed W2 blocks W3.

Pre-blocked routes: REDRESS 92 storage-only-swap framing — W2 must not
land a runtime swap or move a row; REDRESS 60-72 runtime-swap class —
W2 is a compile-time contract, not a runtime swap (P2-B §4).
`[INTEGRATE P3-E]`.

## Section 6 — W3 Union Event-Model — Class-Column Substrate

S-P2 source: `skv9-p2-A-union-event-model.md`. Triumvirate shape:
research → plan → **mandatory CHALLENGE** (substrate-touching) →
redress.

Objective: S-P1 diagnosis #1 — `scan_structurals` is 0.00% self-time;
the SIMD stage-1 index is discarded and the parser re-discovers
structural bytes in a scalar pass. W3 lands the P2-A alternate model:
keep the parser-event cursor stream, add a co-indexed class column on
the existing offset tape at emit time, and consume the SIMD index by
move (Lock 1 substrate cardinality stays at one — no new tape type, no
`UnionTape`). `consume_structural` is deleted; `at_cursor` reads the
class column instead of re-discovering the source byte.

Owner paths (P2-A §5, ~265 hand + ~120 regen LOC):

| Path | Slice | Allowed W3 use |
|---|---|---|
| `runtime/src/tape/{mod,assembler}.rs` | A.1 | Add `classes: Vec<u8>`, `class_at(cursor) -> u8`, `push_offset_with_class`. |
| `runtime/src/grammars/json/parser.rs` | A.2 | `emit_plain_offset` → `emit_event_offset(offset, class)`; move-consumed `idx: u32` cursor on the walker. |
| `runtime/src/grammars/json/generated.rs` | A.3 | Regen: delete `consume_structural` (`generated.rs:292-306`); emit a class ordinal at each emit site. |
| `runtime/src/grammars/json/value.rs` (`JsonNodeKind::at_cursor`) | A.4 | Regen: replace byte-rediscovery (`value.rs:33-46`) with a class-column read — the same-wave production consumer for A.1. |
| `codegen/src/json_templates/{generated,parser,view,value}.rs` | A.5 | Emit the class-column write, the structural-walk lowering, the `class_at` read — the novel-mechanism slice. |
| `bbnf-simd/src/lib.rs` | A.6 | Move-consume API for the structural index + co-written class `Vec<u8>` under `StructuralAlphabet::class_table` (`lib.rs:41`). |
| `runtime/src/grammars/json/scan.rs` | A.7 | Regen: stop discarding the index; surface a move-consume API. |
| `bbnf-simd/tests/checkasm_*.rs` | A.8 | Differential parity tests for the structural-walk lowering. |

Entry gate: W2 closed with `G-W2-RETAINED-PROOF` PASS — the proof
unblocks the union reopen. The W3 plan names the eight slices, the
revert slice per slice, and the W10b regression block.

Exit gate `G-W3-UNION-SUBSTRATE` passes only if:

1. **Must-improve** (P2-A §4.1) — `twitter` Track 1 ≥ 17685 (sonic /
   1.10 floor), `apache_builds` ≥ 14124, `update_center` ≥ 14369; and
   `consume_structural` ≤ 5% self-time on `twitter` / `apache_builds`,
   `JsonNodeKind::at_cursor` ≤ 1% self-time.
2. **Must-not-regress — the W10b six-row block (binding, P2-A §4.2)**:
   `canada` ≥ 15871, `citm_catalog` ≥ 28631, `instruments` ≥ today ×
   0.98, `marine_ik` ≥ 11831, `mesh` ≥ 12186, `numbers` ≥ 17597 —
   per-row `≥ today × 0.98` or the sonic-strict floor, whichever
   binds. Any one row below its floor falsifies the model.
3. `consume_structural` is deleted from `generated.rs`; the class
   column read is present in `at_cursor` — the same-wave consumer is
   wired (P2-A §4.4 #1, #2).
4. Track 2 / `path!` / direct-to-struct / SinkOnly rows show no delta
   beyond noise (no cross-substrate leak — P2-A §4.4 #4).
5. The class column carries only structural ordinals the SIMD producer
   can fill; no `Number`/`Literal` ordinal leaks into the structural
   alphabet (P2-A §4.4 #6).
6. checkasm parity is green for the structural-walk lowering (A.8);
   `checkasm_parity_status=PASS`.
7. Section 2.1 generality scan passes — no JSON-named symbol enters a
   generic crate (P2-A §4.4 #5); the CSS L4 / Sheets / BBNF-self union
   instances compile.
8. Substrate cardinality stays at one — no `UnionTape`, no new
   `BackendShape`, no parser-owned cursor.

`gsoc-2018` does NOT bind W3: it is a P1-named uncloseable row; if it
closes only to ~35000 Mbps that is the residual handed to W4/W5, not a
W3 falsification (P2-A §4.3). W3 falsifies only if the
structural-rediscovery hot leaf does not drop to ≤ 5%.

Revert protocol (P2-A §5): if the §4.2 W10b gate fires, revert the
`assembler.rs` column-push and keep `classes` zero-length — the
substrate compiles with an empty column; revert the four codegen
templates and regen — `generated.rs` / `parser.rs` / `value.rs` return
byte-identical; record the falsified gate in REDRESS and route back to
S-P2/S-P3 without admitting.

Pre-blocked routes: REDRESS 92 storage-only-swap; `UnionTape`, new
`BackendShape`, new directive/BIR, public substrate API, sidecar
substrate, parser-owned cursor/fact slots, `tape_vs_tape` as a
production consumer — all stay blocked. `[INTEGRATE P3-E]`.

## Section 7 — W4 aarch64 ASM Consumers — String-Block Widening

S-P2 source: `skv9-p2-D-aarch64-asm-opportunities.md` §4. Triumvirate
shape: research → plan → **mandatory CHALLENGE** (SIMD primitive) →
redress.

Objective: S-P1 diagnosis #2 — the string-scanner pair reaches 47-67%
self-time on dense-key losses. W4 widens the 16-byte string-block scan
to 32 bytes and rebinds the consumer to the W3 union substrate. The
W3 union is the consumer base — W4 cannot dispatch before W3 closes.

Owner paths (P2-D §4.3, ~300 hand + tests):

| Path | Allowed W4 use |
|---|---|
| `bbnf-simd/src/aarch64/string_block.rs` + `bbnf-simd/src/scalar/string_block.rs` | `scan_string_special_block_32` — 32-byte NEON body + scalar oracle; the producer-side `interesting`-mask OR-fold. |
| `bbnf-simd/tests/checkasm_string_block.rs` (new) | Differential parity gate. |
| `parse-that-regex/src/lib.rs` (`lib.rs:162`) | `match_string_at_quote_trusted_utf8` producer-site rewire to the 32-byte block + scalar tail — the same-wave consumer. |
| `skinny/crates/bbnf-bench/` | `unicode_escapes/direct`, `y_string_unicode/direct`, `unicode_mixed/direct` no-regression CI guard. |

Entry gate: W3 closed with `G-W3-UNION-SUBSTRATE` PASS. The W4 plan
names the 32-byte body, the scalar oracle, and the checkasm gate.

Exit gate `G-W4-STRING-BLOCK` passes only if:

1. The string-dense loss rows lift — `match_string_at_quote` +
   `match_tiny_plain_string` aggregate self-time drops measurably on
   `twitter` / `apache_builds` / `distinct_values`; the W4 plan binds
   the per-row Mbps floors against sonic-strict / 1.10.
2. `scan_string_special_block_32` carries a scalar reference and a
   green `checkasm_string_block.rs` differential test
   (`checkasm_parity_status=PASS`).
3. `match_string_at_quote_trusted_utf8` is rewired to the 32-byte
   block in the same commit — the same-wave consumer; the consumer
   call shows in the `samply` symbol path on the affected rows.
4. The W10b six-row block holds its maintain floor (binding — W4 is a
   string-loop edit).
5. No `direct` plane regression — the three-row CI guard is green.
6. Section 2.1 generality scan passes — `scan_string_special_block_32`
   is a per-string-span-scanner primitive with no JSON structural
   policy (P2-D §4.0).

Revert protocol: if the 32-byte widening does not lift the named
string-dense rows, or if the checkasm gate fails, revert the
`string_block.rs` 32-byte body and the `lib.rs:162` rewire — the
16-byte path is restored; record REDRESS.

Pre-blocked routes: REDRESS 83 (StringBlock16 tiny-probe — W4 must
prove a material differential: the 32-byte widening is producer-side
throughput, not a consumer probe); REDRESS 73 helper-shape transfer —
W4 profiles the producer-site code layout directly. `[INTEGRATE P3-E]`.

## Section 8 — W5 Unicode-Escape Codec — Conditional Admission

S-P2 source: `skv9-p2-E-unicode-escape-codec.md` + `skv9-p2-D-aarch64-asm-opportunities.md`
§3. Triumvirate shape: research → plan → **mandatory CHALLENGE** (SIMD
primitive) → redress.

Objective: S-P1 diagnosis #3 — the unicode-escape codec
(`read_hex_unit_scalar` + `hex_nibble`) is 38-44% self-time on
`y_string_unicode`. W5 lands the `escape_codec_hex_unit` primitive
with five const-generic bindings (JSON-4, CSS L4 variable, JS
variable, TOML-4, TOML-8) plus a NEON kernel. P2-E's honest PMU
verdict: **zero of the four uncloseable rows admit on the codec
alone** — `unicode_escapes` NEAR-FAIL 94.5%, `y_string_unicode`
NEAR-FAIL 94.8%, `unicode_mixed` FAIL 63.7%, `gsoc-2018`
no-regression-basis. Admission is the P2-E §6.3 same-wave conditional
rule: the codec paired with the W4 string-scanner widening. W5 cannot
dispatch before W4 closes.

Owner paths (P2-E §7.1 + P2-D §3.5):

| Path | Allowed W5 use |
|---|---|
| `parse-that-regex/src/lib.rs` (`lib.rs:402`) | Extend `unescape_four_unicode_escapes` from x4-only to all-quartet dispatch; per-quartet NEON fallback to `unescape_uxxxx_neon`. |
| `bbnf-simd/src/aarch64/unescape_uxxxx.rs` + scalar reference | `escape_codec_hex_unit` NEON kernel + scalar oracle (the parity oracle). |
| `codegen/src/` codec template (single file) | The five const-generic binding specialisations. |
| `bbnf-simd/tests/checkasm_unescape.rs` | Differential parity gate. |
| `skinny/crates/bbnf-bench/` | The four-row falsifiability harness. |

Entry gate: W4 closed with `G-W4-STRING-BLOCK` PASS — the string-scanner
widening is the paired knob. The W5 plan states each uncloseable row's
admission basis (`codec_admission_basis`) before redress.

Exit gate `G-W5-CODEC` passes only if:

1. `escape_codec_hex_unit` carries a scalar reference and a green
   `checkasm_unescape.rs` differential test
   (`checkasm_parity_status=PASS`).
2. The codec consumer is wired in the same commit — the per-quartet
   dispatch reaches `unescape_uxxxx_neon`; the consumer shows in the
   `samply` symbol path.
3. Each of the four rows is dispositioned by its P2-E §6.2 basis and
   the disposition is recorded in `codec_admission_basis`:
   `unicode_escapes` admits at 0.90 slack only if the W4 widening
   lifts it past 16319 Mbps (conditional-same-wave);
   `y_string_unicode` admits at the 0.70 W4-precedent slack only if
   the pair clears 8270 Mbps (conditional-same-wave); `unicode_mixed`
   admits only if the paired scanner knob lifts it past 12338
   (conditional-same-wave); `gsoc-2018` admits on the no-regression
   basis (Mbps unchanged, codec neutral).
4. A row that does not clear its paired threshold is recorded
   NEAR-FAIL or FAIL in REDRESS with the honest projection — not
   relabeled as a pass (P2-E §6.4 honest-falsification rule).
5. The W10b six-row block holds its maintain floor.
6. Section 2.1 generality scan passes — the five const-generic
   bindings prove the codec is grammar-neutral (CSS L4 / JS / TOML
   bindings compile).
7. The codec is a `parse_only` gate only — the `direct` plane stays
   behind REDRESS 66-69 + 93 (P2-E §5).

Revert protocol: if no row clears its paired threshold, revert the
`escape_codec_hex_unit` kernel, the codec template, and the
`lib.rs:402` dispatch broadening — the x4-only path is restored;
record REDRESS with the honest per-row projection. W5 may close with
zero strict admissions if every row records NEAR-FAIL/FAIL honestly —
that is a valid measured close, not a paper-close.

Pre-blocked routes: REDRESS 82 (codec falsified on
`unicode_escapes/direct` — W5 must prove a material differential: the
broadening is all-quartet, the gate is `parse_only`); PMULL prefix-XOR
and CTZ/bulk rewires as default hot paths stay blocked — the SHA3
`veor3q_u8` collapse (P2-D §1.2) is Lock-16-gated by FEAT_SHA3 and is
NOT in W5 scope. `[INTEGRATE P3-E]`.

## Section 9 — W6 Close And Alpha Feedback

W6 is a reconciliation wave only, dispatched after W1-W5 dispositions
are recorded. It carries zero source LOC. It reconciles
`skinny/RESULTS.md`, `skinny/REDRESS.md`, this SPEC, `DISPATCH-PROMPT.md`,
`HANDOFF.md`, and any SK-V10 alpha inputs without hiding residual
risk. It records, per uncloseable row, the honest W5 disposition.

Exit gate `G-W6-CLOSE` passes only if the five documents agree, every
wave has an admit or a measured reject in REDRESS, and the §0.1 close
condition is satisfied.

## Section N — G-Gate

`G-ALPHA-SK-V9` — the bracket close gate — passes only when:

1. `G-W0-TELEMETRY-LOCK` is recorded PASS.
2. `G-S-P1-RERUN-CONVERGED` is recorded PASS.
3. `G-BEHAVIOR-RELEASE` passed — W1-W5 each admitted or rejected with
   measurement.
4. `G-W1-TYPED-ADMISSION`, `G-W2-RETAINED-PROOF`,
   `G-W3-UNION-SUBSTRATE`, `G-W4-STRING-BLOCK`, `G-W5-CODEC`, and
   `G-W6-CLOSE` each carry a recorded disposition (PASS or a measured
   REDRESS reject).
5. The §0.1 close condition holds in full.
6. No pre-blocked REDRESS route was reopened without its citation,
   material-difference statement, and CHALLENGE acceptance.
7. The five close documents agree.

On `G-ALPHA-SK-V9` close the orchestrator dispatches Pass Alpha for the
SK-V9 → SK-V10 synthesis per `pass-contracts/PASS-ALPHA.md`.

---

## §5 — Sources

- `restart/skinny/tranches/sk-v9/SPEC.md` (current — §0/§1/§2 structure
  carried forward).
- `restart/skinny/tranches/sk-v9/HANDOFF.md` (candidate boundaries,
  pre-blocked routes, cost binding).
- `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md` (current).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  (the four load-bearing diagnoses; the PMU table; the OLS fit).
- `restart/skinny/tranches/sk-v9/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
  (the six interventions; the dependency order).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`
  (§2 design, §4 falsifiability gate, §5 per-slice cost).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`
  (§1.2 owner files, §2.2 Lock 1, §3.3 audit, §6 LOC envelope).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`
  (§1 REDRESS 91 differential, §2.0 slices, §3 per-row gates, §4 owner
  files).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
  (§3 codec design, §4 string-block widening, §4.3 owner paths).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`
  (§6 falsifiability gate, §6.3 slack rule, §7.1 per-slice LOC).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md`
  (the >SOTA dependency graph; asmjson aarch64 absence).
- `restart/skinny/tranches/sk-v8/SPEC.md` (the SPEC shape mirrored).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (the S-P3 contract).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (the per-wave
  contract the waves conform to).
