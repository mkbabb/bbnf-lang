# SK-V9 P3-F: SPEC Draft — Recovery + Behavior Wave Plan

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-18.
Scope: the next SK-V9 SPEC — §0 close condition + goalset, §0.x
telemetry schema, §1 non-negotiables, §2 wave manifest, §3+ per-wave
sections — folding the S-P1 converged profile, the six S-P2 converged
interventions, and the P3-A..E synthesis cohort into a wave-sequenced,
falsifiability-gated contract.
Output: this file (a DRAFT; the orchestrator promotes it to
`sk-v9/SPEC.md` after S-P3 CHALLENGE converges).
Pass Alpha goalset: SK-V9 §0 close condition carried from the current
SPEC plus the S-P1/S-P2 evidence — the four uncloseable rows, the
structural-rediscovery hot leaf, the Apache/CITM typed-GO lift.
Candidate pool: `research/p2/` post-CHALLENGE survivors (P2-A..F),
distilled by `research/p3/skv9-p3-A-candidate-shortlist.md` into the
eight-candidate shortlist C1..C8.

§0 footer — V2 fold: integrated P3-A..E; all [INTEGRATE] markers
resolved; unified W1-W5 manifest with W4 sub-waved; 10-outcome enum;
36-field schema; live RESULTS floors.

## §0 V3 fold footer

V3 comprehensive integration. Changes in this SPEC: (1) W4b
sub-divided along the P2-E §7.4 slice seams into W4b-1 (scalar
reference + checkasm harness, §7.2.1), W4b-2 (fixed-width codec bodies
+ JSON consumer — the row-moving sub-wave, PAIRED with W4a, §7.2.2),
and W4b-3 (variable-width const-generic bindings + codegen, §7.2.3) —
no single 75-min redress carries the ~1,045-net-LOC codec; the §2
manifest, §2.2 cascade prose, §7 intro, and the §N G-Gate are updated
consistently. (2) W3 risk escalated MEDIUM→HIGH per the P2-A C3 §2.2
warning that the folded P2-D §5 chain raises aggregate risk; W3 is not
sub-waved (the class column and its sole SIMD producer are one
cascade) — it carries a CHALLENGE-gated redress-extension to ≤110 min.
(3) Arithmetic: `update_center` W3 floor `14369 → 14370`
(`ceil(15806/1.10)`); `gsoc-2018` W4b no-regression base `21646 →
22184` live (`RESULTS.md:24`), floor `21430 → 21963`; the W10b six-row
maintain floors are floored uniformly (`floor(today × 0.98)`) — `citm_catalog`
`28631 → 28630`, `numbers` `17597 → 17596`. The W4a + codec pairing is
preserved: W4a pairs with W4b-2.

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
to S-P3. S-P3 distilled them into the C1..C8 shortlist, sequenced them
W1-W5, gated each wave, bound the telemetry schema, and ledgered the
pre-blocked routes. This SPEC is the wave plan that lands them.

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
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-A-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-B-wave-sequencing.md`
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-C-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-D-telemetry-schema.md`
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-E-preblocked-ledger.md`
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
   CHALLENGE; the union reopen (W3) is unblocked by it.
5. Strict admission remains strict-vs-strict on matching output planes
   only; no row admits on stale, permissive, lossy, absent, historical,
   sidecar-only, or view-boundary evidence.
6. The four P1-named uncloseable rows (`unicode_escapes`,
   `unicode_mixed`, `y_string_unicode`, `gsoc-2018`) either admit by
   the W4b conditional same-wave-pairing rule or are recorded
   NEAR-FAIL / FAIL with the honest projection in REDRESS. W4 may close
   with zero strict unicode-row admissions; that is a measured outcome,
   not a paper-close.
7. The W10b six-row regression block (`canada`, `citm_catalog`,
   `instruments`, `marine_ik`, `mesh`, `numbers`) holds its maintain
   floor at every wave and sub-wave that touches the parse loop or an
   aarch64 SIMD kernel.
8. `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SPEC.md`,
   `DISPATCH-PROMPT.md`, and `HANDOFF.md` agree at close.

### Section 0.2 — Goalset (carried from Pass Alpha + S-P1/S-P2 evidence)

The convergent S-P1 verdict (`HARDENING-S-P1-CONVERGED.md` §"Load-bearing
diagnoses"): bbnf's parse-plane losses are **substrate-bound, not
kernel-bound**. Four diagnoses bind the goalset:

| # | S-P1 diagnosis | Goalset target | Owning wave |
|---|---|---|---|
| 1 | `scan_structurals` is 0.00% self-time on every row — the SIMD stage-1 index is discarded; the parser re-discovers structural bytes scalar. | `consume_structural` ≤ 5% self-time on `twitter`, `apache_builds`; `JsonNodeKind::at_cursor` ≤ 1%. | W3 |
| 2 | String-scanner pair (`match_tiny_plain_string` + `match_string_at_quote`) reaches 47-67% self-time on dense-key losses. | 32-byte string-block widening lifts the string-dense losses. | W4a |
| 3 | Unicode-escape codec (`read_hex_unit_scalar` + `hex_nibble`) = 38-44% on `y_string_unicode`. | `escape_codec_hex_unit` SIMD primitive, paired with the W4a scanner widening. | W4b (W4b-1/2/3) |
| 4 | OLS fit `ns_per_byte ≈ 1.079·(q/B) + 0.184·(n/B) + 0.051`, R²=0.371. Four LOSS rows exceed 130-460% of the per-byte budget — delimiter-only intervention is insufficient. | The four uncloseable rows need the codec AND the scanner widening; neither closes them alone (P2-E §6.4). | W4a + W4b-2 paired |

The cheapest GO-count lift is substrate-independent: Apache/CITM
measured typed-row admission (P3-A C1, P2-C) is a mechanical
baseline-whitelist expansion and lands first among the behavior waves.

### Section 0.3 — Opening Baseline And Row Families

The opening benchmark authority is the W0-rendered 38-row JSON report
under `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.

| Family | Row count | SK-V9-open posture |
|---|---:|---|
| `parse_only` | 17 | 17 `S / NO-GO`; baseline rows only; no strict SOTA admission until a wave lifts a named row. |
| `direct_to_struct` | 17 | 3 `A / GO`, 14 `N-direct / NO-GO`; direct digest is a guard plane, not typed product proof. |
| `real_typed_struct` | 4 | 4 `A / GO` (`twitter`, `update_center`, `mesh`, `marine_ik`); Apache/CITM measured typed rows are not present until W1 admits them. |

All current main rows remain `Strictness=deferred`. Native Rust
comparators are same-run; C++ sidecars are historical or absent.

### Section 0.x — Outcome Enum

The SK-V9 outcome enum is the **10-identifier W0-admissible set** that
`validate_w0_outcome` (`report.rs:977-988`) gate-admits:

```text
A   C   G   I   J   K   L   M   N-direct   S
```

- `A` — beat-and-parity (the typed-GO target; current `twitter` typed row).
- `C` — substrate-parity-codegen-acceptable (the GO-without-beat band).
- `G` — substrate failure.
- `I` — parity-oracle disagreement.
- `J` — invalid-input schema rejection.
- `K` — SIMD parity-hash fail (the checkasm differential gate).
- `L` — SIMD throughput fail (the substrate-guard hard-failure axis).
- `M` — memory-residency fail.
- `N-direct` — direct-projection failure (the W0 digest-guard rows).
- `S` — substrate-guard non-admission (the W0 `parse_only` rows;
  `w0_parse_non_admission` demotes admission-capable parse outcomes to
  `S`).

`I`, `J`, and `M` are not optional: `validate_w0_outcome` admits all
ten and the rendered SK-V9-open baseline can carry an `I`/`J`/`M`
verdict. A narrower enum would make `gate-json` reject a row the code
itself produces. SK-V9 mints **no new outcome variant** —
`B D E F-positive F-noise` remain defined in `gate::Outcome` but stay
non-W0-admissible dormant variants; SK-V9 neither uses nor deletes them
(P3-D §3).

### Section 0.y — Required Telemetry Schema

The SK-V9 telemetry schema is the **36-identifier set** P3-D §2.2 pins
— the exact union of the `RowMetadata` schema-v3 fields, the
`SkV8Telemetry` fields, and the `SkV8ComparatorEvidence` fields. The W0
schema is carried forward **unchanged**: **no SK-V9 behavior wave adds
a 37th column** (P3-D §2.1). A wave adding a column without the
matching `validate_schema_v3` check renders a column `gate-json` never
reads; a wave adding a check without the column fails closed.

The 36 gate-consumed required identifiers:

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

Per-wave population obligation (P3-D §2.3): every field a wave emits
into `skinny/RESULTS.md` MUST be consumed by `gate-json` in the **same
wave** — there is no emit-now-consume-later. Symmetrically, `gate-json`
must not require a field the wave does not emit. A field rendered but
not read by `validate_schema_v3` (schema-v3 layer) or
`validate_sk_v8_w0` / `validate_strict_admission` (manifest layer) is a
**producer-only artefact** and fails the wave's exit gate.

The behaviour-class fields carry per-wave values, not new columns:

- `same_wave_consumer_class` — `gate_only` at W0/W1; `<kernel>→<consumer>`
  at any wave landing a kernel (W3, W4a-d).
- `costfacts_*` triad — `none:pre-W1` through W0/W1/W2/W3 unless a
  wave's plan demonstrates a CostFacts-driven shape choice; the row-table
  wave (W1) is explicitly not such a wave (P2-C §2.4, Lock 14).
- `wave_id` — `SK-V9-open` at W0; the per-wave id `sk-v9-real-typed-w{n}`
  / `sk-v9-w{n}` at behaviour waves.
- `run_id` — a fresh `sk-v9-open:criterion-fnv64-<16 hex>` minted per
  behaviour wave under the same `sk-v9-open:` prefix.

No SK-V9 wave promotes a PMU / `cycles_per_byte` / masking-probe /
structural-scan / Criterion-slope figure into the schema. The
`diagnostic_nonproducer_status` field is the fixed constant
`structural_scan+masking_probes+pmu+cycles:nonproducer` and
`validate_sk_v8_w0` hard-rejects any other value — the gate itself
enforces PMU's non-producer status. The PMU table at
`/tmp/skv9-xctrace-v3/pmu_rows.tsv` is a diagnostic input to plan
authoring and per-row Mbps projection only; it is never a gate producer
(P3-D §5).

The schema-version string is `schema-v3 / SK-V9-open` — the
`(SCHEMA_V3_HEADER, wave_id="SK-V9-open")` pair. Behaviour waves do not
bump `schema-v3`; they bump the per-wave `wave_id` and mint a fresh
`run_id` (P3-D §6.1).

### Section 0.z — Comparator Classes

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | sonic-rs strict, serde_json on matching output plane | May support strict admission only if the comparator plane matches the row output plane, `comparator_strictness=strict`, the comparator is a same-run native strict anchor admitted by id, and UTF-8/control/escape validation occurs inside the measured row. |
| Same-run flaw probe | sonic-rs lossy, permissive rows | Planning only; never strict admission. |
| Sidecar planning signal | simdjson, yyjson, RapidJSON; asmjson has no aarch64 backend (P2-F) | Planning only; never strict admission and never an anchored sidecar on this host. |

## Section 1 — Non-Negotiables

- No new directive.
- No new BIR variant.
- No new `BackendShape` variant.
- No new outcome variant — the enum is the 10-identifier W0-admissible
  set (§0.x).
- No new telemetry column — the schema is the 36-identifier set (§0.y).
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
  differential test, and a same-wave hot-path consumer wired in the
  same commit.
- No behavior source change without a same-wave consumer and a
  measured row gate.
- No wave closes on a future-phase promise. "Wired" or "integrated"
  without a bench-row threshold is a paper-close.
- Research, plan, CHALLENGE when first-of-class, redress, and close
  remain distinct commits (`SKINNY-TRIUMVIRATE.md` §9).

## Section 2 — Wave Manifest

The dependency spine is fixed by `HARDENING-S-P2-CONVERGED.md`
§"convergent picture" and the P2-F §7.4 inter-report graph: P2-B proof
→ P2-A union → P2-D consumers; P2-E codec conditional; P2-C fully
independent. P3-A §3 records the cascade-sequencing constraint and the
C1..C8 dependency graph; P3-B §2 sequences the post-W0 behaviour waves.

The post-W0 sequence is **five behaviour brackets** (W1-W5), with W4
sub-waved into four sub-waves (W4a-d). This is inside the ≤12
skinny-bracket ceiling.

| Wave | Section | Name | Shortlist candidate | S-P2 source | Dispatch status | Source LOC budget | Risk | Hard cap |
|---|---|---|---|---|---|---:|---|---:|
| W0 | Section 3 | SK-V9-open Telemetry-Lock Recovery | — | — | Closed | telemetry/gate/report only | — | — |
| W1 | Section 4 | Apache/CITM Measured Typed-Row Admission | C1 | P2-C | Dispatchable — independent, no substrate dependency | ~300 hand | LOW | ≤90 min |
| W2 | Section 5 | Retained Class/Event Grammar + `ValueRef` Proof | C2 | P2-B | Conditional on W1 close + proof-first CHALLENGE | ~425 hand, 0 generated | LOW | ≤90 min |
| W3 | Section 6 | Union Event-Model — Class-Column Substrate | C3 (+ C8 chain) | P2-A + P2-D §5 | Conditional on W2 proof acceptance | ~265 hand + ~120 regen + ~120-220 SIMD chain + ~30-60 VEXT + ~50-90 checkasm | HIGH (CHALLENGE-gated redress extension) | ≤90 min wall / redress 75-min target, ≤110-min CHALLENGE-gated extension |
| W4a | Section 7.1 | 32-byte String-Block Widening | C5 | P2-D §4 | Conditional on W3 close (union substrate is the consumer base) | ~145-270 hand incl. ~40-70 checkasm | MEDIUM | ≤90 min wall / 75-min redress |
| W4b-1 | Section 7.2.1 | `escape_codec` Scalar Reference + Checkasm Harness | C4 (S1/S6) | P2-E §7.1 + P2-D §3 | Conditional on W3 close — lands FIRST, blocks the W4b chain | ~450 hand incl. ~250 checkasm | MEDIUM | ≤90 min wall / 75-min redress |
| W4b-2 | Section 7.2.2 | Fixed-Width Codec Bodies + JSON `unescape_four_unicode_escapes` Consumer | C4 (S2/S3/S5/S7/S8/S11) | P2-E §7.1 + P2-D §3 | PAIRED with W4a; conditional on W4b-1 close — the row-moving sub-wave | ~165 net incl. −215 deletion | MEDIUM-HIGH | ≤90 min wall / 75-min redress |
| W4b-3 | Section 7.2.3 | Variable-Width Const-Generic Bindings + Codegen | C4 (S4/S9/S10) | P2-E §7.1 + P2-D §3 | Conditional on W4b-2 close | ~340 hand | MEDIUM | ≤90 min wall / 75-min redress |
| W4c | Section 7.3 | SHA3 EOR3 Prefix-XOR Ladder | C6 | P2-D §5.3.1 | Conditional on W3 close | ~60-120 hand incl. ~20-40 checkasm | MEDIUM | ≤90 min wall / 75-min redress |
| W4d | Section 7.4 | CSSC CTZ String-Mask Consumer | C7 | P2-D §4.4 | Conditional on W3 close + W4a close | ~15-35 hand | HIGH | ≤90 min wall / 75-min redress |
| W5 | Section 8 | Close And Alpha Feedback | — | — | Conditional on W1-W4 dispositions | docs only | — | ≤90 min |

The `Hard cap` column states the wave wall allowance and the binding
redress sub-cap together: every behaviour wave and W4 sub-wave runs
≤90 min wall with the implementation+measure redress phase fixed at
75 min (60 impl + 15 measure) per `SKINNY-TRIUMVIRATE.md` §7. A wave
whose hand-LOC cannot land in the 75-min redress sub-divides before
dispatch — that is why W4b is itself three sub-waves W4b-1/W4b-2/W4b-3
(§2.2, §7.2), not one ~1,045-net-LOC redress.

LOC budgets are conjunctive with the 90-minute cap. They count
hand-edited source, tests, gate/report code, and hand-written doc
edits the wave names. Generated outputs do not consume the source LOC
budget, but every generated file is named, diff-audited, and included
in the revert slice. A wave plan exceeding either bound splits before
dispatch or returns REVISE.

Phase caps per `SKINNY-TRIUMVIRATE.md` §7: Research 30 min × ≤6 agents;
Plan 30 min; CHALLENGE 90 min when first-of-class or substrate-touching;
Redress 75 min (60 impl + 15 measure). The W4 sub-wave structure exists
precisely so that no redress overruns the 75-min ceiling — see §2.2.

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
- Non-JSON proof: every generic-crate edit (the W3 SIMD chain + codegen
  template, the W4a `string_block.rs`, the W4b `escape_codec`
  primitive, the W4c EOR3 ladder, the W4d CTZ extract) carries a
  CSS L4 / Sheets / BBNF-self proof — a named no-op dry run, focused
  test, or unchanged-output audit. P2-A names the CSS L4 / Sheets /
  BBNF-self union instances; P2-B names the Sheets `EventGrammar`
  witness; P2-E names the five const-generic codec bindings (JSON-4,
  CSS L4 variable, JS variable, TOML-4, TOML-8).

### Section 2.2 — The Cascade-Lock, Disambiguated

P2-D §0 reads: a P2-D consumer kernel "may not be split" from the union
substrate — "P2-A must land in the same wave as any of these P2-D
consumer slices, or the slices fall back to REDRESS-rejected
parser-owned shapes." The V1 drafts read this three ways. The binding
reading:

> The constraint means a P2-D kernel must not land **without the union
> substrate existing**. It does NOT mean one monolithic redress wave.

It is satisfied by **W3 (the union event-model) preceding W4a-d**. By
the time any W4 sub-wave dispatches, the W3 union class column and the
move-consumed structural index are landed and live — the kernel's
production consumer (the union substrate) exists. Each W4 sub-wave's
redress commit then wires its kernel into that already-landed W3 union
**in the same commit**: the consumer exists, the caller is wired
same-commit, no orphan ships. A monolithic codec+string-block+ASM wave
would be ~1,595-1,860 LOC (P3-A C4 ~1,045 net + C5 ~145-270 + C6
~60-120 + C7 ~15-35 + the C8 checkasm files) and cannot complete in a
75-min redress (CH4 #19). The sub-wave structure resolves this without
violating the cascade — each sub-wave is a fresh triumvirate,
individually inside its LOC budget and its 75-min redress cap.

**W4b is itself three sub-waves.** The `escape_codec_hex_unit` codec
(C4) is ~1,045 net LOC across the eleven P2-E §7.4 slices — ~6.0 h
aggregate per-slice cap. One 75-min redress cannot land it; sub-waving
the *bracket* W4a-d did not sub-divide the *codec*. W4b is therefore
cut along the P2-E §7.4 slice seams into three sub-waves, each inside
its own 75-min redress:

- **W4b-1** (§7.2.1) — the `escape_codec/scalar.rs` reference (S1) +
  `escape_codec/mod.rs` kernel surface + the `checkasm_escape_codec.rs`
  differential harness (S6). The parity foundation; lands FIRST and
  blocks the W4b chain. ~450 hand incl. ~250 checkasm.
- **W4b-2** (§7.2.2) — the fixed-width NEON bodies `hex_x4_neon.rs`
  (JSON-4 + TOML-4, S2), `hex_x8_neon.rs` (TOML-8, S3), and
  `surrogate_join.rs` (S5), plus the JSON production consumer
  re-body of `unescape_four_unicode_escapes` (S7), the sink call-site
  swap (S8), and the −215 superseded-kernel deletion (S11). This is
  **the sub-wave that moves the unicode rows** — it carries the W4b
  conditional-admission gate and is **PAIRED with W4a** (§7.2.2,
  P2-E §6.4). ~165 net LOC.
- **W4b-3** (§7.2.3) — the variable-width const-generic body
  `hex_variable_neon.rs` (CSS L4, JS `\u{}`, S4), the CSS L4 scaffold
  (S9), and the `codegen/src/escape_codec/` const-generic emission
  (S10). The grammar-neutrality breadth slice. ~340 hand LOC.

The W4a + W4b pairing is preserved exactly: W4a pairs with **W4b-2**
— the codec moves no row to GO without the string-block widening, and
W4b-2 is the codec sub-wave that carries the row-moving consumer
(P2-E §6.4). W4b-1 and W4b-3 carry no row gate. The codec admits as a
checkasm-verified primitive at W4b-1; the rows admit, per-row on
measurement, at W4b-2.

**W3 redress cap.** W3 at ~265 hand + ~120 regen + the P2-D §5 SIMD
chain (~120-220 SIMD + ~30-60 VEXT + ~50-90 checkasm) is ~465-635
hand-equivalent + ~120 regen — ~1.5-2× the W1 ~300-LOC/~85-min-redress
scale. It plausibly overruns the 75-min redress sub-cap once the §5
structural-bitmap chain is folded in. W3 is **not** sub-waved: the
union substrate (A.1-A.5) and the SIMD structural-bitmap producer
(A.6-A.8 + P2-D §5) form one cascade — splitting them orphans the
class column from its only producer for the duration of the gap,
and the SPEC §1 same-wave-consumer non-negotiable forbids that. W3
instead carries an honest **CHALLENGE-gated redress-extension note**:
the W3 redress targets the 75-min sub-cap; if the W3 plan's slice
estimate shows it cannot land in 75 min, the W3 CHALLENGE may grant a
single redress extension to ≤110 min, recorded in the CHALLENGE
disposition, with the orchestrator surfacing the extension decision to
the user per `SKINNY-TRIUMVIRATE.md` §7. W3's risk is **HIGH** — P2-A
C3 §2.2 warned the folded P2-D §5 chain raises the wave's aggregate
risk from MEDIUM to HIGH; that escalation is recorded here, in §2, and
in §6.

Three distinct "same-wave" relations are in play; they are named
distinctly throughout this SPEC to prevent the V1 conflation:

1. **Cascade-lock** — a P2-D kernel lands only after the W3 union
   substrate exists (this section).
2. **Same-wave consumer** — every primitive and its hot-path caller
   land in one commit (Section 1, the orphan-kernel non-negotiable).
3. **Codec/scanner pairing** — W4a and W4b are strictly adjacent
   sub-waves because neither closes the four uncloseable rows alone
   (Section 7.2, P2-E §6.4).

## Section 3 — W0 SK-V9-open Telemetry-Lock Recovery

Status: closed. Close artifact:
`restart/skinny/tranches/sk-v9/research/skv9-W0-close.md`.
Run id: `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.

W0 made the opening telemetry self-consistent as `SK-V9-open`,
`gate-json`-consumed, and froze all behavior surfaces. `G-W0-TELEMETRY-LOCK`
PASSED: the manifest carries exactly the 38 main row identities, one
uniform run id, no Apache/CITM/Canada measured typed rows, behavior
freeze paths unchanged, and structural-scan/masking/PMU/cycles
diagnostic non-producers. W0 populated all 36 schema fields for the
38-row baseline with the pre-behaviour constants. W0 is not redispatched
unless a later CHALLENGE finds a concrete telemetry-lock defect and
names a revert/redress slice.

## Section 4 — W1 Apache/CITM Measured Typed-Row Admission

Shortlist candidate C1. S-P2 source: `skv9-p2-C-apache-citm-admission.md`.
Triumvirate shape: research (archived P2-C cohort) → plan → redress;
CHALLENGE optional — this is a mechanical baseline-whitelist expansion,
not first-of-class. LOC ~300 hand (P2-C §2.0); risk LOW; hard cap
≤90 min, ≤85-min redress estimate (P2-C §2.0).

Objective: REDRESS 91 admitted Apache/CITM source/product parity but
not measured rows because the `SK_V8_OPEN_BASELINE` whitelist was not
expanded with W2's admission (P2-C §1). W1 captures a fresh same-run
Criterion row for Apache and CITM, flips the gate test assertions, and
promotes two `real_typed_struct A / GO` rows.

Owner paths (the seven P2-C paths; any other source path returns
REVISE before editing):

| Path | Allowed W1 use |
|---|---|
| `skinny/crates/bbnf-bench/src/report.rs:709` | Expand `SK_V8_OPEN_BASELINE` with the Apache/CITM admission (possible rename to `SK_V9_OPEN_BASELINE`). |
| `skinny/crates/bbnf-bench/src/bin/gate.rs:1820-1831` | Flip the `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures` Apache/CITM assertions from `!w0_real_typed_metadata_expected(...)` to `w0_real_typed_metadata_expected(...)`. |
| `skinny/RESULTS.md` | Promoted row block — two `real_typed_struct A / GO` rows + two schema-v3 telemetry rows; run-id refresh across the file. |
| `skinny/crates/bbnf-bench/target/skv9-w1/criterion/` | Fresh same-run capture (out-of-band; non-LOC artefact). |
| `skinny/REDRESS.md` | New entry recording the promotion, fresh run-id, the no-regression guard on the four existing typed GO rows, the Track 2/oracle independence claim, per-row throughput. |
| `restart/skinny/tranches/sk-v9/HANDOFF.md` | Move the candidate from "may admit" to "admitted under SK-V9 W1". |
| `restart/locks/LOCKS.md` (Lock 14) | Add `sk-v9-real-typed-w1` parent-diff allowance scoped to the seven owner paths. |

Entry gate: W0 closed; `G-S-P1-RERUN-CONVERGED` PASS; the W1 plan
names the fresh capture method and the per-row Mbps falsifiability
threshold.

Exit gate `G-W1-TYPED-ADMISSION` passes only if:

1. A fresh same-run Criterion capture is produced across the 21 existing
   rows + the 8 typed Criterion ids with `RUSTFLAGS="-C target-cpu=native"`.
2. Apache `track1_real_typed_struct` ≥ `ceil(sonic_apache_typed / 1.10)`
   — the ±9.1% strict slack the four existing typed GO rows hold
   (twitter +0.7%, update_center −4.5%, mesh +4.6%, marine_ik +25.2%,
   `skinny/RESULTS.md:7,18,21,28`).
3. CITM `track1_real_typed_struct` ≥ `ceil(sonic_citm_typed / 1.10)` at
   the same slack.
4. The four existing `real_typed_struct A / GO` rows maintain GO with
   no regression below `sonic-strict / 1.10`.
5. The promoted rows carry independent Track 2 or oracle evidence
   (`assert_real_typed_parity`, `real_typed_struct.rs:310-323`); no row
   admits on Track 1 ≡ Track 2.
6. `cargo xtask gate-json --advisory --check-results` succeeds after
   the RESULTS promotion.
7. Section 2.1 generality scan passes — no JSON policy enters a generic
   crate.

Maintain envelope: the four typed-GO rows hold `A / GO`; the two
existing direct rows (`apache_builds/direct_to_struct` N-direct/NO-GO,
`citm_catalog/direct_to_struct` A/GO) hold their SK-V9-open verdicts.
The W10b WIN-block is not gated here — W1 touches no parse loop
(P3-C §2 W1).

Revert protocol: if the gate test refuses to compile or the assertion
flip exposes baseline/fixture drift, revert both assertions to
`!expected`; if `gate-json --check-results` fails after promotion,
revert `RESULTS.md` to the pre-promotion run-id snapshot; if Lock 14
`lock14_baseline` fails, revert the LOCKS.md allowance and route the
wave through a Lock 14 amendment. Record REDRESS; do not close by
prose. Does not block any later wave — W1 is row-table-only.

Pre-blocked routes (P3-E §2.2 / §3.1, verbatim):

- REDRESS 91 — W1 is the later accepted row-table wave REDRESS 91
  explicitly deferred to. Material differential: REDRESS 91's gap is a
  *whitelist*, not an architecture; W1 owns a fresh run-id/metadata
  validation and produces measured rows under it, then expands the
  whitelist. W1 admits **Apache + CITM only**.
- REDRESS 80 + the canada long-decimal mismatch — `canada/real_typed_struct`
  stays rejected; W1 does not touch the f64 path.
- REDRESS 93 — W1 touches no direct guard plane.
- REDRESS 60-72, 85-87, Lock 14 — W1 extends only the existing typed
  schema/generator path; no retained-parse sidecar, no JSON policy in
  a generic crate.

## Section 5 — W2 Retained Class/Event Grammar + `ValueRef` Proof

Shortlist candidate C2. S-P2 source: `skv9-p2-B-retained-grammar-proof.md`.
Triumvirate shape: research → plan → **mandatory CHALLENGE** (first-of-class
proof surface) → redress. LOC ~395-425 hand, 0 generated (P2-B §1.2 /
§6.1); risk LOW; hard cap ≤90 min.

Objective: REDRESS 92 rejected the SK-V8 W3 union before source redress
because the scanner/tape event model was not isomorphic. REDRESS 92
routed the precursor: define the retained class/event grammar including
numbers/literals and string quote ownership, prove the retained
`ValueRef` cursor contract over it. W2 lands that proof — a
*compile-time* `EventGrammar` trait, an `AnyGrammar` default, a
`ValueRef<'tape, 'src, G>` cursor, and JSON + Sheets witnesses behind
`#[cfg(any(test, feature = "proof"))]`. Proof-only depth: **zero
`RESULTS.md` row movement**, zero generated output, zero production
consumer. The proof exists to unblock the W3 union reopen.

Owner paths (P2-B §1.2 / §6.1):

| Path | Role |
|---|---|
| `skinny/crates/runtime/src/tape/event_grammar.rs` (NEW) | `EventGrammar` trait + `AnyGrammar` default instance. |
| `skinny/crates/runtime/src/tape/event_grammar_tests.rs` (NEW, in `tests/`) | The `_proof_compiles` triple + the negative `ValueRef<'static, 'static, …>` compile-fail test. |
| `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs` (NEW) | JSON `impl EventGrammar`. |
| `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs` + `mod.rs` (NEW) | Sheets `impl EventGrammar` — the Lock 14 non-JSON witness. |
| `skinny/crates/runtime/src/tape/mod.rs` (TOUCHED, ~20 lines) | `ValueRef` `K = AnyKind` → `G: EventGrammar = AnyGrammar`. |
| `skinny/crates/runtime/src/lib.rs` (TOUCHED, ~5 lines) | Re-exports behind one `#[cfg(any(test, feature = "proof"))]` at the parent `pub mod` site. |
| `skinny/crates/runtime/Cargo.toml` (TOUCHED, 1 line) | Declare the `proof` feature named by the cfg gate; no dependency or default-feature change. |

Witness proof files are `cfg`-gated; the default build keeps production parser
behavior identical.
No file under `grammars/json/parser.rs|generated.rs|scan.rs|value.rs`
or `codegen/` is touched.

Entry gate: W1 closed; `G-S-P1-RERUN-CONVERGED` PASS; the W2 plan
states the proof is compile-time and names the `cfg` isolation.

Exit gate `G-W2-RETAINED-PROOF` passes only if (P3-C §3.1):

1. The `EventGrammar` trait compiles, is grammar-neutral by signature
   (no `match grammar` arm, no role enum), and
   `cargo check -p runtime --features proof` is green.
2. The three `const _: fn() = _proof_compiles::<G>` witness lines for
   `JsonEventGrammar`, `SheetsEventGrammar`, and `AnyGrammar` all
   compile; the negative `ValueRef<'static, 'static, JsonEventGrammar>`
   test is rejected by the borrow checker.
3. The default build `cargo build -p runtime` is behavior-identical to the
   pre-W2 build — witness modules are fully behind `cfg`, and the only default
   surface change is the zero-sized `ValueRef` marker bound/default rename.
4. The Lock 14 `rg` audits (P2-B §3.3) report every `admits_fact` /
   `admits_class` match inside `event_grammar.rs`, a witness file, or
   the proof test — never in generic substrate source.
5. `skinny/RESULTS.md` is byte-identical — the proof moved zero rows.
6. `rg 'event_grammar|event_grammar_witness' skinny/crates/bbnf-bench/`
   returns zero — the witnesses cannot be reached by `cargo bench`.
7. The proof-first CHALLENGE accepted the owner paths, the LOC budget,
   the revert slice, and the no-production-consumer posture.

Maintain envelope: structural, not Mbps — no `RESULTS.md` row moves;
no edit to `generated.rs`/`scan.rs`/`parser.rs`/`value.rs`/templates;
the witness files excluded from the release library (P3-C §3.2).

Revert protocol (P2-B §6.1): five slices (S1 trait, S2 JSON witness,
S3 Sheets witness, S4 `ValueRef` parameterisation, S5 cfg gating +
proof tests), each its own revert unit. If S4 leaks to call sites,
back out and reattempt with a `pub type` alias. A failed proof (any
`const _` line refuses to compile) reverts the whole wave — there is
no partial proof. A failed W2 blocks W3: the W3 class-column design
would otherwise re-open REDRESS 92.

Pre-blocked routes (P3-E §2.3 / §3.2, verbatim):

- REDRESS 92 — W2 is the routed precursor REDRESS 92 named, not a
  reopen. Five-axis differential (P2-B §4): no production consumer, no
  row-movement surface, touches none of the parser-control files
  REDRESS 60-72 touched, adds no payload field, proves the existing
  `ValueRef` cursor rather than modifying control flow.
- REDRESS 50, 51, 53 — no parser-written aux side table, no
  parser-local byte-class or structural-mask cursor; W2 edits no
  production parser path.
- REDRESS 60-72 — W2 does not reopen the retained-parse candidate
  ledger; it is proof-only depth.
- REDRESS 71 — orthogonal; the admitted typed-DirectBuild route, not
  the retained-tape contract.

## Section 6 — W3 Union Event-Model — Class-Column Substrate

Shortlist candidate C3, with the P2-D §5 dead-scanner structural-bitmap
chain folded in per P3-A §1.1 (the §5 chain is the structural-bitmap
producer body C3's union event-model consumes — the same kernel, the
same wave; it is NOT a separate candidate). S-P2 source:
`skv9-p2-A-union-event-model.md` + `skv9-p2-D-aarch64-asm-opportunities.md`
§5. Triumvirate shape: research → plan → **mandatory CHALLENGE**
(substrate-touching) → redress. LOC ~265 hand + ~120 regen (P2-A §5.9)
+ ~120-220 SIMD `bbnf-simd` chain + ~30-60 VEXT + ~50-90 `scan_structurals`
checkasm (P2-D §5) — ~465-635 hand-equivalent + ~120 regen aggregate;
risk **HIGH** — the contracting mechanism deletes `consume_structural`
and the codegen-template structural-walk lowering is the novel surface,
and P2-A C3 §2.2 records that folding the P2-D §5 structural-bitmap
chain in whole raises the wave's aggregate risk from MEDIUM to HIGH
(the chain is a wave-class substrate body, not a leaf kernel). Hard cap
≤90 min wall; the 75-min redress sub-cap is the target, with a
**CHALLENGE-gated redress extension to ≤110 min** admissible if the W3
plan's slice estimate demonstrates the union substrate + the §5 SIMD
chain cannot co-land in 75 min (§2.2). W3 is not sub-waved — the
class column and its sole SIMD producer form one cascade and the SPEC
§1 same-wave-consumer rule forbids landing the column without the
producer.

Objective: S-P1 diagnosis #1 — `scan_structurals` is 0.00% self-time;
the SIMD stage-1 index is discarded and the parser re-discovers
structural bytes in a scalar pass. W3 lands the P2-A alternate model:
keep the parser-event cursor stream, add a co-indexed class column on
the existing offset tape at emit time, consume the SIMD index by move
(Lock 1 substrate cardinality stays at one — no new tape type, no
`UnionTape`), and fold in the P2-D §5 structural-bitmap producer body
(4-register TBL classify + quote/escape/backslash mask + VEXT
cross-chunk carry). `consume_structural` is deleted; `at_cursor` reads
the class column instead of re-discovering the source byte.

Owner paths (P2-A §5 eight slices + the P2-D §5 chain):

| Path | Slice | Allowed W3 use |
|---|---|---|
| `runtime/src/tape/{mod,assembler}.rs` | A.1 | Add `classes: Vec<u8>`, `class_at(cursor) -> u8`, `push_offset_with_class`. |
| `runtime/src/grammars/json/parser.rs` | A.2 | `emit_plain_offset` → `emit_event_offset(offset, class)`; move-consumed structural-index walker cursor. |
| `runtime/src/grammars/json/generated.rs:292-306` | A.3 | Regen: delete `consume_structural`; structural-index walk. |
| `runtime/src/grammars/json/value.rs:29-47` | A.4 | Regen: `JsonNodeKind::at_cursor` byte-rediscovery → class-column read — the same-wave production consumer for A.1. |
| `codegen/src/json_templates/{generated,parser,view,value}.rs` | A.5 | Class-column emission + structural-walk lowering — the novel-mechanism slice. |
| `bbnf-simd/src/lib.rs` + `bbnf-simd/src/aarch64/` | A.6 | Surface the structural index by move-consume; the P2-D §5 structural-bitmap chain (TBL classify, quote/escape/backslash mask, VEXT carry). |
| `runtime/src/grammars/json/scan.rs` | A.7 | Regen: stop discarding the index; move-consume API. |
| `bbnf-simd/tests/checkasm_scan_structurals.rs` (NEW) + `bbnf-bench/src/parity.rs` | A.8 | `scan_structurals` end-to-end checkasm + corpus-parity gate; class-column parity + structural-index move-consumed asserts. |

Entry gate: W2 closed with `G-W2-RETAINED-PROOF` PASS — the proof
unblocks the union reopen; the REDRESS 92 gating clause is discharged.
The W3 plan names the eight slices, the per-slice revert, and the W10b
regression block.

Exit gate `G-W3-UNION-SUBSTRATE` passes only if (P3-C §2 W3,
sonic-strict floors derived live from `skinny/RESULTS.md`):

1. **Must-improve** — the seven GO-target structural-dense `parse_only`
   Track 1 rows cross the standard-parity floor `ceil(sonic_strict / 1.10)`:
   `twitter` ≥ 17685 (today 13188, sonic 19453, `RESULTS.md:5`);
   `apache_builds` ≥ 14124 (today 11917, sonic 15536, `:12`);
   `update_center` ≥ 14370 (today 9857, sonic 15806, `ceil(15806/1.10)`, `:16`);
   `distinct_values` ≥ 15731 (today 8972, sonic 17304, `:39`).
   Plus the structural hot-leaf falsifiers: `consume_structural` ≤ 5%
   self-time on `twitter` / `apache_builds`; `JsonNodeKind::at_cursor`
   ≤ 1% self-time.
2. **Must-not-regress — the W10b six-row block (binding, P2-A §4.2)** —
   each `parse_only` Track 1 at `floor(today × 0.98)` or
   `ceil(sonic_strict / 1.10)`, whichever higher. The `today × 0.98`
   leg is **floored** uniformly across all six rows — the single
   rounding convention for the whole W10b block: `canada` ≥ 15866
   (today 16190, sonic 12723 — `floor(today × 0.98)` binds,
   `RESULTS.md:10`); `citm_catalog` ≥ 28630 (today 29215,
   `floor(28630.7)`, `:8`); `instruments` ≥ 15865 (today 16189,
   `floor(15865.2)`, `:29`); `marine_ik` ≥ 11831 (today 12073,
   `floor(11831.5)`, `:26`); `mesh` ≥ 12186 (today 12435,
   `floor(12186.3)`, `:19`); `numbers` ≥ 17596 (today 17956,
   `floor(17596.9)`, `:31`). Any one row below its floor falsifies the
   model.
3. `consume_structural` is deleted from `generated.rs`; the class
   column read is present in `at_cursor` — the same-wave consumer is
   wired same-commit (P2-A §4.4 #1, #2). CH5 falsifier: `rg
   'consume_structural' skinny/crates/runtime/src/` returns zero
   outside the deletion-commit diff.
4. Track 2 / `path!` / direct-to-struct / SinkOnly rows show no delta
   beyond noise (no cross-substrate leak — P2-A §4.4 #4).
5. The class column carries only structural ordinals the SIMD producer
   can fill; no `Number`/`Literal` ordinal leaks into the structural
   alphabet (P2-A §4.4 #6).
6. checkasm parity is green for the `scan_structurals` chain (A.8).
7. Section 2.1 generality scan passes — no JSON-named symbol enters a
   generic crate (P2-A §4.4 #5); the CSS L4 / Sheets / BBNF-self union
   instances compile.
8. Substrate cardinality stays at one — no `UnionTape`, no new
   `BackendShape`, no parser-owned cursor.

`gsoc-2018` does NOT bind W3: it is a P1-named uncloseable row carrying
a unicode-bearing residual; if it closes only partially that is the
residual handed to W4, not a W3 falsification (P2-A §4.3). W3 falsifies
only if the structural-rediscovery hot leaf does not drop to ≤ 5%.

Revert protocol (P2-A §5): slices A.3/A.4/A.7 are regen output —
reverting the codegen-template commit (A.5) rolls back four downstream
files; `generated.rs` returns to the `consume_structural` shape
byte-identically. If the §4.2 W10b gate fires, revert the
`assembler.rs` column-push and keep `classes` zero-length — the
substrate compiles with an empty column. A full W3 revert blocks
W4a-d: the union substrate is the cascade-locked consumer base. A
*partial* W3 (class column lands, exit rows NEAR-MISS) does not block
W4 — the substrate is the dependency, not the exit-gate Mbps (P3-C §2
W3 revert).

Pre-blocked routes (P3-E §2.4 / §3.2, verbatim):

- REDRESS 92 — W3 implements the routed precursor (the alternate
  event-model), not the rejected storage-only swap; gated behind the
  W2 proof.
- REDRESS 50 — the class column is co-emitted at the existing
  `emit_plain_offset` call site; not a parser-written aux side table.
  Falsifier: any pass other than the parser writing `tape.classes`.
- REDRESS 51 — no parser-local byte-class cursor; `ParserState` gains
  no cursor field beyond `state.cursor` + the structural-index walker
  idx.
- REDRESS 53 — no parser-local structural-mask cursor / second
  scanner; the structural index is consumed by move.
- REDRESS 60-72 — no retained-parse sidecar producer; the class
  column is the tape's own column, one producer.
- REDRESS 83, 84, 88, 89 — orthogonal; W3 changes no string-scanner
  pair and leaves the SIMD producer's Layer-1 vocabulary unchanged.
- The REDRESS 92 blanket pre-block — no new `BackendShape`/BIR/directive,
  no public substrate API, no parser-owned structural cursor/facts, no
  `tape_vs_tape` production consumer, no `UnionTape`, no Tier B
  string-boundary work.

## Section 7 — W4 aarch64 Substrate Consumers

W4 is the substrate-consumer bracket, sub-waved into six sub-waves —
W4a, the three codec sub-waves W4b-1/W4b-2/W4b-3 (§7.2), W4c, and W4d.
Each W4 sub-wave is a fresh triumvirate per `SKINNY-TRIUMVIRATE.md`
§1 (research → plan → mandatory CHALLENGE → redress); each sub-wave's
redress wires its kernel into the **already-landed W3 union substrate**
in the same commit (the cascade-lock, §2.2); each row-moving sub-wave
carries the W10b six-row maintain gate. The sub-wave structure is what
keeps every redress inside the 75-min ceiling — a monolithic W4 would
be ~1,595-1,860 LOC (§2.2), and the codec alone is ~1,045 net (hence
the three-way W4b split).

### Section 7.1 — W4a 32-byte String-Block Widening

Shortlist candidate C5. S-P2 source: `skv9-p2-D-aarch64-asm-opportunities.md`
§4. LOC ~145-270 hand incl. ~40-70 `checkasm_string_block.rs` (P2-D
§4.3); risk MEDIUM (the µop-neutral-per-byte finding — the win is
consumer-side mask-handling halving, so the gate measures the combined
path); hard cap ≤90 min.

Objective: S-P1 diagnosis #2 — the string-scanner pair reaches 47-67%
self-time on dense-key losses. W4a widens the 16-byte string-block scan
to 32 bytes (`scan_string_special_block_32`) at the existing producer
call site.

Owner paths (P2-D §4.3):

| Path | Allowed W4a use |
|---|---|
| `bbnf-simd/src/aarch64/string_block.rs` | `scan_string_special_block_32` — 32-byte NEON body + the `interesting`-mask producer-side OR-fold. |
| `bbnf-simd/src/scalar/string_block.rs` | 32-byte scalar oracle. |
| `parse-that-regex/src/lib.rs:162` | `match_string_at_quote_trusted_utf8` producer-site rewire to the 32-byte block + scalar tail — the same-wave consumer. |
| `bbnf-simd/tests/checkasm_string_block.rs` (NEW) | Differential parity gate over the 16-byte and 32-byte bodies (the C8 file REDRESS 83's rejected patch never landed). |

Entry gate: W3 closed with `G-W3-UNION-SUBSTRATE` PASS — the union
substrate exists (cascade-lock satisfied). The W4a plan names the
32-byte body, the scalar oracle, and the checkasm gate; the checkasm
file lands FIRST and blocks the wave.

Exit gate `G-W4a-STRING-BLOCK` passes only if:

1. The combined producer + consumer path lifts the string-dense rows —
   `unicode_mixed` (today 6803, `RESULTS.md:33`) and `gsoc-2018` (today
   22184, `:24`) measured against the named floors; the W4a plan binds
   the per-row Mbps floors against `ceil(sonic_strict / 1.10)`.
2. `scan_string_special_block_32` carries a scalar reference and a
   green `checkasm_string_block.rs` differential test (outcome `K` on
   any parity miss).
3. `match_string_at_quote_trusted_utf8` is rewired to the 32-byte
   block in the same commit; the consumer call shows in the `samply`
   symbol path on the affected rows.
4. The W10b six-row maintain gate holds (binding — W4a is a string-loop
   edit; the floors are §6 exit-gate clause 2).
5. No `unicode_escapes/direct`, `y_string_unicode/direct`,
   `unicode_mixed/direct` regression — the three-row CI guard is green.
6. Section 2.1 generality scan passes — `scan_string_special_block_32`
   is a per-string-span scanner with no JSON structural policy.

Revert protocol: the checkasm gate lands FIRST and blocks all wiring;
if the 32-byte body fails parity, revert the body and the `lib.rs:162`
rewire — the 16-byte path is restored. If the combined path does not
lift the named rows, the row stays NO-GO and W4a records the measured
contribution in REDRESS. If the W10b block regresses, revert the
producer rewire. A W4a revert does not block W4b-d; but W4b is
strictly paired with W4a (§7.2).

Pre-blocked routes (P3-E §2.5 / §3.5, verbatim):

- REDRESS 83 — material differential: different call site (the *full*
  `match_string_at_quote_trusted_utf8` path, not the tiny 16-byte-cap
  probe); a successor 32-byte primitive replacing the 16-byte primitive
  at the producer site, not a JSON-specific wrapper; same-wave consumer
  is the existing `match_string_at_quote_trusted_utf8`. Binding gate:
  the falsification must measure the combined producer + consumer path,
  not the block-scan microbench.
- REDRESS 60, 61, 62 — no retained trusted-string boundary collapse;
  the widening is producer-side per-block.

### Section 7.2 — W4b `escape_codec_hex_unit` Codec — Conditional Admission

Shortlist candidate C4. S-P2 source: `skv9-p2-E-unicode-escape-codec.md`
+ `skv9-p2-D-aarch64-asm-opportunities.md` §3. The codec is ~1,045 net
LOC across the eleven P2-E §7.4 slices (~890 hand + ~120 regen + ~250
tests − 215 deletion) — ~6.0 h aggregate per-slice cap. One 75-min
redress cannot land it. W4b is therefore **three sub-waves** —
W4b-1/W4b-2/W4b-3 — cut along the P2-E §7.4 slice seams (§2.2), each a
fresh triumvirate inside its own LOC budget and 75-min redress cap.

Objective: S-P1 diagnosis #3 — the unicode-escape codec
(`read_hex_unit_scalar` + `hex_nibble`, `parse-that-regex/src/lib.rs:945-966`)
is 38-44% self-time on `y_string_unicode`. W4b lands the
`escape_codec_hex_unit` primitive with five const-generic bindings
(JSON-4, CSS L4 variable, JS `\u{}`, TOML-4, TOML-8) and re-bodies the
already-wired x4 JSON path onto it.

**Pairing.** P2-E §6.4 is the binding honest verdict: **zero of the
four uncloseable rows admit on the codec alone.** The codec alone moves
no row to GO; a standalone codec wave would paper-close. The
row-moving sub-wave is **W4b-2** — it carries the fixed-width JSON
codec bodies and the JSON production consumer — and it is **PAIRED
with W4a**, strictly adjacent and never separable: `unicode_mixed`
admission is conditional on the W4a string-block widening being live in
the same redress lineage. W4b-1 (the parity foundation) and W4b-3 (the
variable-width breadth) carry no row gate.

#### Section 7.2.1 — W4b-1 `escape_codec` Scalar Reference + Checkasm Harness

Shortlist candidate C4, slices S1 + S6 + `mod.rs` (P2-E §7.1). LOC ~450
hand incl. ~250 `checkasm_escape_codec.rs`; risk MEDIUM (the const-generic
kernel surface and the parity-oracle scalar body — LOW on correctness,
MEDIUM on the const-generic dispatcher shape); hard cap ≤90 min wall /
75-min redress.

Objective: land the codec parity foundation that every later W4b
sub-wave is diffed against — the `escape_codec/scalar.rs` reference
(the parity oracle), the `escape_codec/mod.rs` const-generic kernel
surface + dispatcher, and the `checkasm_escape_codec.rs` differential
harness. W4b-1 ships no NEON body and moves no row; it is the
scalar-reference + checkasm precondition the SK-V5 orphan-kernel
discipline mandates before any kernel wires.

Owner paths (P2-E §7.1 slices S1, S6, mod.rs):

| Path | Allowed W4b-1 use |
|---|---|
| `bbnf-simd/src/aarch64/escape_codec/scalar.rs` (NEW) | The scalar reference re-homed from `read_hex_unit_scalar` + `hex_nibble` — the parity oracle for all NEON bodies. |
| `bbnf-simd/src/aarch64/escape_codec/mod.rs` (NEW) | Const-generic kernel surface + dispatcher; the five-binding parameter shape. |
| `bbnf-simd/tests/checkasm_escape_codec.rs` (NEW) | Differential parity harness covering all five const-generic bindings; the gate every W4b-2/W4b-3 body must clear. |

Entry gate: W3 closed with `G-W3-UNION-SUBSTRATE` PASS. The W4b-1 plan
names the const-generic parameter set and the checkasm case enumeration
(digit-count × alignment × terminator × validity, ~6,000 cases per
binding, P2-E §7.3).

Exit gate `G-W4b-1-CODEC-HARNESS` passes only if:

1. `escape_codec/scalar.rs` compiles and `cargo test -p bbnf-simd`
   exercises it as a standalone reference — no NEON body present yet.
2. `checkasm_escape_codec.rs` compiles and runs green against the
   scalar reference for every binding; the harness IS the same-wave
   consumer for the scalar body (the test consumes the reference).
3. `escape_codec/mod.rs` exposes the const-generic surface for all five
   bindings; the dispatcher is grammar-neutral by signature.
4. Section 2.1 generality scan passes — the kernel surface embeds no
   JSON structural policy; the bindings are opaque const-generic
   parameters.

W4b-1 moves no row; it has no W10b maintain obligation beyond compiling
clean (it ships no parse-loop edit). Revert protocol: all three files
are NEW — revert the files on any failure; W4b-2 cannot dispatch until
W4b-1 closes (the checkasm harness is W4b-2's admission precondition).

#### Section 7.2.2 — W4b-2 Fixed-Width Codec Bodies + JSON Consumer

Shortlist candidate C4, slices S2 + S3 + S5 + S7 + S8 + S11
(P2-E §7.1). LOC ~165 net (~150 `hex_x4` + ~140 `hex_x8` + ~50
`surrogate_join` + ~30 consumer re-body + ~10 sink swap − 215
deletion); risk MEDIUM-HIGH (LOW on JSON-4 correctness,
MEDIUM-HIGH on `unicode_escapes` / `y_string_unicode` performance,
HIGH on `unicode_mixed`); hard cap ≤90 min wall / 75-min redress.

**This is the row-moving W4b sub-wave.** W4b-2 lands the fixed-width
NEON codec bodies and re-bodies the JSON `unescape_four_unicode_escapes`
production consumer onto them — the path that actually moves the unicode
rows. **W4b-2 is PAIRED with W4a — strictly adjacent, never separable**
(P2-E §6.4): neither the codec nor the string-block widening closes the
four uncloseable rows alone, so W4b-2 dispatches only with W4a landed.

Objective: land `hex_x4_neon.rs` (JSON-4 + TOML-4 fixed-width body),
`hex_x8_neon.rs` (TOML-8), and `surrogate_join.rs` (the scalar pair-join
algebra), re-body the already-wired x4 JSON path at
`parse-that-regex/src/lib.rs:402`, swap the `runtime/src/grammars/json/sink.rs`
call site, and delete the superseded `unescape_uxxxx.rs` kernel (−215,
lands LAST after the consumer is green).

Owner paths (P2-E §7.1 slices S2, S3, S5, S7, S8, S11):

| Path | Allowed W4b-2 use |
|---|---|
| `bbnf-simd/src/aarch64/escape_codec/hex_x4_neon.rs` (NEW) | Fixed-4 NEON body — JSON `\u`, TOML `\u`. |
| `bbnf-simd/src/aarch64/escape_codec/hex_x8_neon.rs` (NEW) | Fixed-8 NEON body — TOML `\U` (compile-validated this sub-wave, no production consumer). |
| `bbnf-simd/src/aarch64/escape_codec/surrogate_join.rs` (NEW) | Scalar UTF-16 pair-join algebra (JSON Pair binding). |
| `parse-that-regex/src/lib.rs:402` / `:718-810` | Re-body the already-wired `unescape_four_unicode_escapes` x4 path + the `Some(b'u')` arm onto the kernel — the production consumer. |
| `runtime/src/grammars/json/sink.rs` | Call-site swap. |
| `bbnf-simd/src/aarch64/unescape_uxxxx.rs` | Superseded kernel removed (−215 LOC, lands LAST after the consumer is green). |

Entry gate: W4b-1 closed with `G-W4b-1-CODEC-HARNESS` PASS (the scalar
reference + checkasm harness are live); W4a closed (the paired scanner
widening is live). The W4b-2 plan states each uncloseable row's
`codec_admission_basis` before redress.

Exit gate `G-W4b-2-CODEC` — the conditional-admission rule (P3-C §4,
P2-E §6, sonic-strict floors live from `skinny/RESULTS.md`). W4b-2
admits **per-row, on measurement**:

1. `unicode_escapes` Track 1 ≥ 16319 (standard parity, sonic-strict
   18132 × 0.90, `RESULTS.md:35`). Projected 15423 — **NEAR-FAIL 94.5%**
   on the codec alone. Admits as `A / GO` iff the *measured* post-wave
   Mbps clears the floor.
2. `y_string_unicode` Track 1 ≥ 8270 (W4-precedent structural-hard
   slack, sonic-strict 11814 × 0.70, `RESULTS.md:41`). Projected 7837 —
   **NEAR-FAIL 94.8%**. Admits iff measured clears.
3. `unicode_mixed` Track 1 ≥ 12338 (standard 0.85, sonic-strict 14515,
   `RESULTS.md:33`). The codec touches only ~10% of this row's c/B;
   the codec alone projects 7864 — **FAIL 63.7%**. Admits iff the
   *combined* W4a string-block + W4b-2 codec measured Mbps clears 12338.
   If W4a did not land, `unicode_mixed` stays NO-GO and W4b-2 admits
   codec-contribution-only — never claimed closed by the codec.
4. `gsoc-2018` Track 1 ≥ 21963 — **no-regression basis** (`ceil(live
   baseline 22184 × 0.99)`, `RESULTS.md:24`; codec c/B share ≈ 0%; the
   row's load is the string-block scanner). The no-regression floor is
   derived from the live SK-V9-open `RESULTS.md` figure, the same
   22184 §7.1 uses for the W4a string-block clause — one baseline per
   row. Closing `gsoc-2018` is out of scope for the codec.
5. `hex_x4_neon` / `hex_x8_neon` / `surrogate_join` each clear the
   `checkasm_escape_codec.rs` harness W4b-1 landed — outcome `K` on any
   parity miss.
6. The §3.5 direct-route no-regression gate on `unicode_escapes/direct`,
   `y_string_unicode/direct`, `unicode_mixed/direct` holds — REDRESS
   82's blocking rows become W4b-2's admission/no-regression rows.
7. The W10b six-row maintain gate holds — W4b-2 re-bodies the JSON
   unescape hot path; the six floors are §6 exit-gate clause 2.
8. Section 2.1 generality scan passes — the fixed-width bodies embed no
   JSON structural policy.

**The honest posture (P2-E §6.4, carried verbatim).** W4b-2 may close
with **zero strict unicode-row admissions**. A NEAR-FAIL on
`unicode_escapes` / `y_string_unicode` is the expected, honestly
projected outcome — the row stays `S / NO-GO`, the measured codec
contribution is recorded in REDRESS, the residual routes forward, and
the sub-wave still admits the codec body as a checkasm-verified
primitive. That is an honest measured outcome, **not a paper-close**.
W4b-2 is reverted wholesale only on a checkasm parity failure or a
W10b WIN-block regression — never on a per-row NEAR-MISS.

Revert protocol (P2-E §7.1): the W4b-1 checkasm harness is the gate; if
a NEON body fails parity, revert that body and the JSON consumer falls
back to the scalar reference S1. If a row's conditional gate fails, the
row stays NO-GO and the codec contribution is recorded. If the W10b
block regresses, revert the consumer re-body. The −215 deletion lands
LAST and reverts independently. A W4b-2 revert does not block W4b-3 or
W4c/W4d.

Pre-blocked routes (P3-E §2.5 / §3.4, verbatim):

- REDRESS 82 — material differential (five axes): not a parser-owned
  per-quartet classifier (the 4-quartet batched path is the
  union-substrate path; the single-quartet binding fires only on
  pre-filter reject); the same-wave consumer is the already-wired x4
  JSON path at `lib.rs:402`; `escape_codec_hex_unit` is a const-generic
  primitive with five bindings; the evidence is post-V3 PMU self-time;
  the falsification gate is `parse_only` only.
- REDRESS 64 — no retained Unicode-escape run validator; the kernel is
  pure functional.
- REDRESS 66-69 + 93 — W4b-2's gate is `parse_only` only; it does not
  enter the direct plane / DirectBuild semantic string facts.
- REDRESS 88, 89 — orthogonal; the codec is not the prefix-XOR / CTZ
  path.

#### Section 7.2.3 — W4b-3 Variable-Width Const-Generic Bindings + Codegen

Shortlist candidate C4, slices S4 + S9 + S10 (P2-E §7.1). LOC ~340 hand
(~180 `hex_variable_neon` + ~40 CSS L4 scaffold + ~120 codegen);
risk MEDIUM (the variable-width body is a code path not present today —
checkasm covers all 1..6 widths × validity × terminator positions);
hard cap ≤90 min wall / 75-min redress.

Objective: complete the codec's grammar-neutrality breadth — land the
variable-width `hex_variable_neon.rs` body (CSS L4 `\HHHHHH`,
JS `\u{}`), the CSS L4 `#[cfg(test)]` scaffold, and the
`codegen/src/escape_codec/` const-generic emission for all five
bindings. W4b-3 ships no production consumer beyond the codegen
template; CSS L4 and JS remain compile-validated scaffolds (no
production parse loop, no falsifiability gate).

Owner paths (P2-E §7.1 slices S4, S9, S10):

| Path | Allowed W4b-3 use |
|---|---|
| `bbnf-simd/src/aarch64/escape_codec/hex_variable_neon.rs` (NEW) | Variable-width NEON body — CSS L4, JS `\u{}`. |
| `codegen/src/escape_codec/` (NEW sub-module) | Const-generic emission for the five bindings (directory module per `feedback_directory_modules`). |
| `bbnf-css/tests/` | CSS L4 scaffold (`#[cfg(test)]`, compile-only). |

Entry gate: W4b-2 closed with `G-W4b-2-CODEC` PASS (the fixed-width
bodies + checkasm harness are live). The W4b-3 plan names the
variable-width checkasm extension.

Exit gate `G-W4b-3-CODEC-BINDINGS` passes only if:

1. `hex_variable_neon` clears the `checkasm_escape_codec.rs` harness
   across all 1..6 widths × validity × terminator positions — outcome
   `K` on any parity miss.
2. The `codegen/src/escape_codec/` const-generic emission compiles and
   the five emitted specialisations are diff-audited; the codegen
   template is the same-wave consumer for the const-generic surface.
3. The CSS L4 `#[cfg(test)]` scaffold compiles — the Lock 14 non-JSON
   binding witness; it carries no production parse loop and no row gate.
4. Section 2.1 generality scan passes — the five const-generic bindings
   prove grammar-neutrality (CSS L4 / JS / TOML bindings compile).

W4b-3 moves no row; it carries no W10b maintain obligation beyond
compiling clean (the variable-width body has no JSON production
consumer). Revert protocol: all owner paths are NEW files or a NEW
sub-module — revert on any failure; the hand-written W4b-2 fixed-width
bodies remain callable. A W4b-3 revert does not block W4c/W4d.

Pre-blocked routes (P3-E §2.5 / §3.4, verbatim):

- REDRESS 82 — the variable-width body is a const-generic primitive
  binding, not a parser-owned per-quartet classifier; the codegen
  emission is template-driven, not a JSON-instance.
- REDRESS 85-87 + Lock 14 — the CSS L4 scaffold carries the non-JSON
  proof Section 2.1 mandates; no JSON policy enters the codegen shell.

### Section 7.3 — W4c SHA3 EOR3 Prefix-XOR Ladder

Shortlist candidate C6. S-P2 source: `skv9-p2-D-aarch64-asm-opportunities.md`
§5.3.1. LOC ~60-120 hand incl. ~20-40 checkasm extension (P2-D §5.3.1);
risk MEDIUM (the vector-ladder representation differs from the u64-word
scalar representation — the parity oracle covers the three-way
differential); hard cap ≤90 min.

Objective: accelerate the W3 union-substrate structural-bitmap producer.
W4c replaces the scalar shift-XOR ladder inside `bitmap_prefix_xor_64`
with a `uint8x16_t` shift-XOR ladder using `veor3q_u8` 3-stage fold
(6-stage XOR → 3 EOR3 ops, ~12 µops → ~6 µops), gated by a Lock-16
`FEAT_SHA3` host-capability predicate. C6 is a **producer accelerator**
— it moves no row of its own; its speed-up surfaces inside W3's
must-improve rows.

Owner paths (P2-D §5.3.1):

| Path | Allowed W4c use |
|---|---|
| `bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs` | Vector `uint8x16_t` shift-XOR ladder with `veor3q_u8` 3-stage fold + Lock-16 `FEAT_SHA3` admissibility gate. The scalar shift-XOR ladder stays the unconditional fallback. |
| `bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs` (EXISTS) | Extend with the EOR3 path under a forced `FEAT_SHA3` mask — the vector-vs-scalar-vs-PMULL three-way differential. |

Entry gate: W3 closed with `G-W3-UNION-SUBSTRATE` PASS — the
structural-bitmap producer (the EOR3 ladder's only consumer) is live.

Exit gate `G-W4c-EOR3` passes only if:

1. The EOR3 ladder carries a green three-way `checkasm_bitmap_prefix_xor_64.rs`
   differential (vector-vs-scalar-vs-PMULL); outcome `K` on a parity
   miss.
2. A `cargo asm` proof that `veor3q_u8` (not `pmull`) appears under
   `-C target-cpu=native` with `FEAT_SHA3` present.
3. The W10b six-row maintain gate holds — this is the **binding** gate;
   the prefix-XOR hot body is the surface W10b proved regresses the WIN
   block even when correctness-green. No EOR3 body ships unless all six
   rows hold their floor.
4. The EOR3 path is `FEAT_SHA3`-conditional with the scalar shift-XOR
   ladder as the unconditional fallback — a capability-conditional
   specialisation, the same admissibility shape as `digit_mac`
   (DotProd-gated).
5. Section 2.1 generality scan passes.

Revert protocol: the checkasm extension lands before the wiring;
checkasm failure blocks the wave. Reverting the EOR3 body is a
predicate-flip (the scalar ladder is the unconditional fallback), not a
parse-loop rollback. If the W10b block regresses, revert to
scalar-only. A W4c revert does not block any later sub-wave.

Pre-blocked routes (P3-E §2.6 / §3.3, verbatim):

- REDRESS 88 — material differential (three axes): different intrinsic
  (EOR3 is a 3-input bitwise XOR, no carryless multiply); different
  latency profile (PMULL.1Q 4-cycle vs EOR3 1-cycle — the REDRESS-88
  retire-latency failure mode is structurally inapplicable); different
  primitive shape (a vector shift-XOR ladder fold of the scalar ladder
  REDRESS 88 *kept*, not the PMULL substitution). EOR3 accelerates the
  scalar ladder; it does not re-admit PMULL. PMULL stays rejected as
  the default body.
- HANDOFF §5 "PMULL prefix-XOR as default hot paths" — applies to PMULL
  re-admission, explicitly not to the SHA3-gated wave-internal EOR3
  fold.

### Section 7.4 — W4d CSSC CTZ String-Mask Consumer

Shortlist candidate C7. S-P2 source: `skv9-p2-D-aarch64-asm-opportunities.md`
§4.4. LOC ~15-35 hand (P2-D §4.4 — the smallest candidate); risk HIGH
(REDRESS 89 already rejected the structurally-adjacent CSSC CTZ body;
the differential is plausible but unproven); hard cap ≤90 min.

Objective: accelerate the W4a 32-byte block scanner's per-mask
first-set extract. W4d replaces the consumer-side `<u16>::trailing_zeros`
(`rbit + clz` under the production baseline) with a CSSC `ctz` under
`-C target-cpu=native`. C7 is a **consumer accelerator** — it moves no
row of its own; its contribution surfaces inside W4a's combined-path
gate on `gsoc-2018` and `unicode_mixed`.

Owner paths (P2-D §4.4):

| Path | Allowed W4d use |
|---|---|
| `bbnf-simd/src/aarch64/` mask consumer | The CSSC CTZ body at the string-mask first-set extract + a `cargo asm` proof the intended CTZ sequence appears. The `rbit + clz` form stays the unconditional fallback. |

W4d's correctness is exercised by W4a's `checkasm_string_block.rs` (the
CTZ extract is a sub-step of the 32-byte block scanner's mask consumer);
no separate checkasm file.

Entry gate: W3 closed (the union-substrate string-mask consumer is the
non-orphan condition) **and** W4a closed (the 32-byte block scanner the
CTZ extracts from is live). W4d is the deepest sub-wave — it needs both.

Exit gate `G-W4d-CTZ` passes only if:

1. A `cargo asm` proof that `ctz` emits under `-C target-cpu=native` —
   the instruction-selection gate.
2. The W10b six-row maintain gate holds — this is the **binding hard
   blocking precondition**; REDRESS 89 rejected the structurally
   adjacent CSSC CTZ body precisely on a 3-8% WIN-block regression.
3. The CTZ body is host-capability-gated at the non-default call site
   with the `rbit + clz` fallback unconditional.
4. Section 2.1 generality scan passes.

Revert protocol: if the W10b block regresses, revert the CTZ body and
fall back to `rbit + clz`. A W4d revert is a predicate-flip. W4d is the
terminal substrate sub-wave; a revert routes the kernel back under a
fresh REDRESS material-differential.

Pre-blocked routes (P3-E §2.6 / §3.6, verbatim):

- REDRESS 89 — material differential: different call site (the
  string-mask first-set extract, not the `bulk_emit_positions_64`
  structural-scan bulk consumer); different failure profile (LOSS rows
  under guard, not the WIN-block numeric rows); same-wave consumer is
  the union-substrate string-mask consumer (W3 + W4a scope). Binding
  gate: the W10b six-row maintain gate is the hard blocking
  precondition.
- HANDOFF §5 "CTZ/bulk production rewires as default hot paths" —
  applies to default rewires; W4d is a host-capability-gated
  specialisation at a non-default call site.

## Section 8 — W5 Close And Alpha Feedback

W5 is a reconciliation wave only, dispatched after W1-W4 dispositions
are recorded. It carries zero source LOC and no CHALLENGE. It reconciles
`skinny/RESULTS.md`, `skinny/REDRESS.md`, this SPEC, `DISPATCH-PROMPT.md`,
`HANDOFF.md`, and any SK-V10 alpha inputs without hiding residual risk.
It records, per uncloseable row, the honest W4b disposition (admit /
NEAR-FAIL / FAIL with the measured contribution). It verifies the
36-field schema renders identically across all admitted/rejected rows.

Exit gate `G-W5-CLOSE` passes only if the five documents agree, every
W1-W4 wave and sub-wave has an admit or a measured reject in REDRESS,
and the §0.1 close condition is satisfied.

## Section N — G-Gate

`G-ALPHA-SK-V9` — the bracket close gate — passes only when:

1. `G-W0-TELEMETRY-LOCK` is recorded PASS.
2. `G-S-P1-RERUN-CONVERGED` is recorded PASS.
3. `G-BEHAVIOR-RELEASE` passed — W1-W4 (W4a, W4b-1/W4b-2/W4b-3, W4c,
   W4d) each admitted or rejected with measurement.
4. `G-W1-TYPED-ADMISSION`, `G-W2-RETAINED-PROOF`, `G-W3-UNION-SUBSTRATE`,
   `G-W4a-STRING-BLOCK`, `G-W4b-1-CODEC-HARNESS`, `G-W4b-2-CODEC`,
   `G-W4b-3-CODEC-BINDINGS`, `G-W4c-EOR3`, `G-W4d-CTZ`, and
   `G-W5-CLOSE` each carry a recorded disposition (PASS or a measured
   REDRESS reject).
5. The §0.1 close condition holds in full — including clause 6: W4 may
   close with zero strict unicode-row admissions if every uncloseable
   row records NEAR-FAIL / FAIL honestly.
6. No pre-blocked REDRESS route was reopened without its citation,
   material-difference statement, and CHALLENGE acceptance.
7. The five close documents agree.

On `G-ALPHA-SK-V9` close the orchestrator dispatches Pass Alpha for the
SK-V9 → SK-V10 synthesis per `pass-contracts/PASS-ALPHA.md`.

---

## §Sources

- `restart/skinny/tranches/sk-v9/SPEC.md` (current — §0/§1/§2 structure
  carried forward).
- `restart/skinny/tranches/sk-v8/SPEC.md` (the prior precepts/tranche
  SPEC shape mirrored).
- `restart/skinny/tranches/sk-v9/HANDOFF.md` (candidate boundaries,
  pre-blocked routes, cost binding).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  (the four load-bearing diagnoses; the PMU table; the OLS fit).
- `restart/skinny/tranches/sk-v9/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
  (the six interventions; the dependency order).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A..F` (the six
  converged S-P2 reports).
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-A-candidate-shortlist.md`
  (C1..C8; the dependency graph; proof-only vs row-moving).
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-B-wave-sequencing.md`
  (the post-W0 wave manifest; the topological + risk-graded order).
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-C-falsifiability-gates.md`
  (the per-wave gates; the three slack rules; the W4b conditional rule).
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-D-telemetry-schema.md`
  (the 36-field schema; the 10-outcome enum; the same-wave-consumption
  rule; the PMU non-producer disposition).
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-E-preblocked-ledger.md`
  (the per-wave pre-block lists; the 5 material differentials; the 13
  hard pre-blocks; the W10b six-row block).
- `skinny/RESULTS.md` (the 38-row SK-V9-open baseline; per-row Track 1
  and sonic-strict Mbps — the live floors).
- `skinny/REDRESS.md` (the pre-blocked routes).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (the S-P3 contract).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (the per-wave
  contract the waves conform to).
