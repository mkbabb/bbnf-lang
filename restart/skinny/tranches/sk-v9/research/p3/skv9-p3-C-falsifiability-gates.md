# SK-V9 P3-C: Per-Wave Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-18.
Scope: For every SK-V9 post-W0 wave in the P3-B manifest, author the
falsifiability gate — exit gate (named corpus rows + Mbps thresholds),
maintain envelope (the W10b WIN-block + the 4 typed-GO + 3 direct-GO
rows), revert protocol, same-wave-consumer requirement, and the
proof-only-wave gate shape.
Output: this file.
Pass Alpha goalset: SK-V9 §0 close condition — strictly beat sonic-rs +
simdjson NEON + yyjson on every parse_only and real_typed_struct row on
Apple M5 Max under matched RFC-8259 strictness; each post-W0 behaviour
candidate admits by a measured row gate or remains explicitly blocked
in REDRESS.
Candidate pool: `research/p2/` post-CHALLENGE survivors (P2-A union
event-model, P2-B retained-grammar proof, P2-C Apache/CITM admission,
P2-D aarch64 ASM kernels, P2-E unicode codec, P2-F SOTA teardown).

---

## §1 — Method

### §1.1 — What a falsifiability gate is

Per `PASS-3-SYNTHESIS-PLAN.md` §9 and the CH1 / CH6 lens registry, a
falsifiability gate is **measurable from the bench**. An unmeasurable
gate is rejected. Every gate in §2 below carries four mandatory parts:

1. **Exit gate** — the measurable admission condition. Named corpus
   rows + concrete Mbps thresholds, compared against the `SK-V9-open`
   baseline (`sk-v9-open:criterion-fnv64-cd1673844eeea12f`) on the
   strict plane. A wave whose exit gate is "wired" / "integrated"
   without a bench-row threshold is a paper-close and fails CH6.
2. **Maintain envelope** — the rows the wave must NOT regress. Every
   gate carries the W10b six-row WIN-block plus, where the wave touches
   a typed or direct codepath, the four typed-GO and three direct-GO
   rows. A breach falsifies the wave even if the exit gate clears.
3. **Revert protocol** — what rolls back if the exit gate fails, and
   whether the failure blocks subsequent waves.
4. **Same-wave consumer** — the hot-path caller that lands in the same
   commit as any primitive. An orphan kernel (primitive without its
   consumer) fails CH4 + CH6 per the SK-V5 orphan-kernel failure and
   the dav1d four-tuple discipline.

### §1.2 — The three slack rules (the comparator-threshold vocabulary)

The gates below use three threshold forms, each grounded in a cited
artefact. They are stated here once and referenced by name in §2/§4.

| Slack rule | Threshold form | Provenance | When it applies |
|---|---|---|---|
| **Standard parity** | `Mbps_new ≥ ceil(sonic_strict / 1.10)` | `DIRECT_PROJECTION_SONIC_SLACK = 1.10` (`gate.rs:56`); the 1.10 ns-slack the four existing typed-GO rows already clear | Any row whose dominant cycle sink is the primitive the wave designs (P2-E §6.3 standard 0.90; P2-C §4.3 typed). |
| **W4-precedent structural-hard** | `Mbps_new ≥ 0.70 × sonic_strict` | SK-V7 W4 / REDRESS 82 gate; bound to a primary-source structural-hardness criterion (P2-E §6.3) | A single row — `y_string_unicode` — whose corpus shape (99%+ short 6-byte `\uXXXX`, 40.5% codec c/B) leaves it codec-bound even under an ideal codec. |
| **No-regression basis** | `Mbps_new ≥ Mbps_baseline − 1%` | P2-E §6.3; the W10b maintain-floor `today × 0.98` per P2-A §4.2 | A row whose bottleneck is NOT the wave's primitive (codec ≈0% share); and every row in the maintain envelope. |

The strict comparator on M5 Max is **sonic-rs strict, same-run native**
(P2-F §1: simdjson NEON and yyjson are historical sidecars, asmjson is
`n/a`/non-anchored). All exit-gate thresholds in §2 are sonic-strict.

### §1.3 — Honesty constraint inherited from S-P2

P2-E §6.4 established — by rederivation from
`/tmp/skv9-xctrace-v3/pmu_rows.tsv` — that **zero of the four
uncloseable rows admit on the unicode codec alone**. The V1 fabricated
c/B column claiming `unicode_escapes PASS` was rejected; the rederived
posture is `unicode_escapes NEAR-FAIL 94.5%`, `y_string_unicode
NEAR-FAIL 94.8%`, `unicode_mixed FAIL 63.7%`, `gsoc-2018 no-regression
only`. The §4 conditional-admission rule encodes this verdict directly:
the codec wave's gate is a **same-wave conditional rule**, not an
aspirational "codec closes 4 rows."

### §1.4 — The candidate→wave map this gate set assumes

P3-B authors the wave manifest; P3-C gates each wave. The candidate set
this artefact gates (per the S-P3 prompt's candidate enumeration) maps:

| Wave | Candidate | Primitive class | Row-moving? |
|---|---|---|---|
| W0 | telemetry-lock (closed) | none | no — baseline |
| W1 | P2-C — Apache + CITM typed row-table admission | none (row-table only) | yes — 2 new typed rows |
| W2 | P2-B — retained class/event grammar + `ValueRef` proof | none (compile-time contract) | **no — proof-only** |
| W3 | P2-A — union event-model (class column; `consume_structural` deletion) | structural projection | yes — structural-dense rows |
| W4 | P2-E codec + P2-D §4 string-block widening (paired) | `escape_codec_hex_unit` + `scan_string_special_block_32` | yes — unicode rows |
| W5 | P2-D §5 aarch64 ASM kernels (EOR3 ladder, CSSC CTZ, structural-bitmap) | SIMD substrate kernels | yes — string-dense rows |

W3 (union substrate) is the topological precursor: P2-D §3.5/§4.4/§5.4
and P2-E's union-substrate consumer all block on the union substrate.
The codec wave W4 is sequenced with the string-block widening **paired
in the same wave** because P2-E §6.3 makes `unicode_mixed` admission
conditional on a same-wave per-string-span scanner intervention. P3-B
owns final sequencing; P3-C's gates are stated against this candidate
set and re-bind to whatever wave letters P3-B assigns.

---

## §2 — Per-wave gate table

Each row is one wave. Mbps figures are `SK-V9-open` parse_only Track 1
unless flagged typed. Thresholds cite the slack rule from §1.2.

### W1 — Apache + CITM Typed Row-Table Admission (P2-C)

| Field | Specification |
|---|---|
| **Exit gate rows + thresholds** | `apache_builds/real_typed_struct/main` Track 1 ≥ `ceil(sonic_apache_typed / 1.10)` (standard parity). `citm_catalog/real_typed_struct/main` Track 1 ≥ `ceil(sonic_citm_typed / 1.10)` (standard parity). Both rows must additionally pass full-fixture checksum parity bbnf ≡ serde ≡ sonic (`assert_real_typed_parity`, `real_typed_struct.rs:310-323`). Citation: P2-C §3.1/§3.2/§4.3. The wave produces a fresh same-run Criterion capture (21 existing rows + 8 new typed Criterion ids) under run-id `sk-v9-open:` or `sk-v9-w{n}:` per P2-C §2.3. |
| **Maintain envelope** | (a) The four typed-GO rows — `twitter`, `update_center`, `mesh`, `marine_ik` `real_typed_struct` Track 1 — must hold their `A / GO` outcome: each ≥ `sonic_strict / 1.10` (no-regression vs SK-V9-open typed baseline at `report.rs:718-724, 795-801, 810-816, 853-859`). (b) The two existing direct rows — `apache_builds/direct_to_struct` (N-direct/NO-GO) and `citm_catalog/direct_to_struct` (A/GO) — must hold their SK-V9-open verdicts unchanged (the wave touches no direct codepath). (c) The W10b WIN-block is **not gated** here — W1 is a row-table-only wave, touches no parse loop, allocates no parse-loop work; the six WIN rows are unaffected by construction and a no-regression check is vacuous. |
| **Revert protocol** | Per P2-C §4.3: any failed gate → halt at the redress phase, record the falsified gate in REDRESS, revert the two `SK_V8_OPEN_BASELINE` entries, the gate-test assertion flips, and the `RESULTS.md` promotion to the pre-promotion run-id snapshot. The four typed-GO rows act as guards and hold their A/GO outcome unchanged. **Does not block subsequent waves** — W1 is row-table-only; W2-W5 do not depend on Apache/CITM measured rows. A revert routes the candidate back to S-P2/S-P3 without admitting the row. |
| **Same-wave consumer** | None required — this is a row-table-only wave producing no primitive. The "consumer" is the gate itself (`gate_only` consumer class per P2-C §2.4). The Criterion capture is the artefact. |

### W2 — Retained class/event grammar + `ValueRef` cursor proof (P2-B)

Proof-only wave — gate detailed in §3. No row movement.

### W3 — Union Event-Model: class column + `consume_structural` deletion (P2-A)

| Field | Specification |
|---|---|
| **Exit gate rows + thresholds** | Structural-dense parse_only Track 1 rows must cross the standard-parity floor (`sonic_strict / 1.10`). Per P2-A §4.1: `twitter` ≥ 17685 (today 13188); `apache_builds` ≥ 14124 (today 11917); `update_center` ≥ 14369 (today 9857); `distinct_values` ≥ 15731 (today 8972). **Plus** the structural hot-leaf falsifiers (P2-A §4.4 #1-2): `consume_structural` self-time ≤ 5% on `twitter` + `apache_builds` post-wave; `JsonNodeKind::at_cursor` ≤ 1% self-time (the per-cursor byte-rediscovery must be deleted, not retained). `gsoc-2018` partially closes (its 51% gap is also unicode-bearing; full closure routes to W4) — `gsoc-2018` is **not** an exit-gate row for W3, only a no-regression row. |
| **Maintain envelope** | The W10b six-row WIN-block is the **binding** maintain gate (P2-A §4.2, §4.4 #3): `canada` ≥ 15866 (`today × 0.98`; sonic floor binds higher); `citm_catalog` ≥ 28631; `instruments` ≥ `today × 0.98`; `marine_ik` ≥ 11831; `mesh` ≥ 12186; `numbers` ≥ 17597. Any one of the six below its maintain floor falsifies the wave even if every exit row clears — this is the verbatim WIN block the SK-V8 W10/W10b campaign regressed 3-8% even when correctness-green. `citm_catalog` is the load-bearing guard (the most class-write-dense row). Additionally (P2-A §4.4 #4): Track 2 / `path!` / direct-to-struct / SinkOnly rows show no delta beyond noise — the class column touches only retained-view consumers. For parity with the W1 envelope, and so the gate is mechanically checkable, that no-leak clause is bound to the seven GO rows by name: the four typed-GO rows — `twitter`, `update_center`, `mesh`, `marine_ik` `real_typed_struct A / GO` — and the three direct-GO rows — `citm_catalog`, `marine_ik`, `unicode_basic` `direct_to_struct A / GO` — each hold their `A / GO` outcome with no delta beyond noise. |
| **Revert protocol** | Per P2-A §5: slices A.3/A.4/A.7 are regen output — reverting the codegen-template commit (slice A.5) rolls back four downstream files in one move; `generated.rs` returns to the `consume_structural` shape byte-identically. If the W10b gate fires, revert the `assembler.rs` column-push and keep the `classes` field zero-length (slice A.1 revert) — the substrate compiles with an empty column. **Blocks W4 and W5 if it fully reverts**: P2-D §3.5/§4.4/§5.4 and P2-E's union-substrate consumer all name the union substrate as their same-wave consumer; absent the class column, those slices fall back to REDRESS-rejected parser-owned shapes. A *partial* W3 (class column lands, exit rows NEAR-MISS) does not block W4/W5 — the substrate is the dependency, not the exit-gate Mbps. |
| **Same-wave consumer** | The retained `JsonRoot` view's `JsonNodeKind::at_cursor` (`value.rs:33-46`) is re-bodied to read `tape.class_at(cursor)` instead of the source-byte rediscovery — this IS the production consumer for the class column, landing in the same commit (P2-A §2.4 #2). The byte-rediscovery line is deleted, not left dead. CH5 falsifier: `rg 'consume_structural' skinny/crates/runtime/src/` returns zero matches outside the deletion-commit diff. |

### W4 — Unicode codec + string-block widening, PAIRED (P2-E + P2-D §4)

| Field | Specification |
|---|---|
| **Exit gate rows + thresholds** | Conditional-admission rule — see §4 in full. Summary: `unicode_escapes` Track 1 ≥ 16319 (standard parity, sonic 18132 × 0.90); `y_string_unicode` Track 1 ≥ 8270 (W4-precedent structural-hard, sonic 11814 × 0.70); `unicode_mixed` Track 1 ≥ 12338 (standard 0.85, sonic 14515) — **admissible only with the same-wave string-block widening landed**; `gsoc-2018` Track 1 ≥ 21430 (no-regression basis, `21646 − 1%`). The wave admits **per-row**: each row admits iff its *measured* post-wave Mbps clears its threshold. P2-E §6.4 projects zero of four cross the gate on the codec alone — the gate is honest about this (§4). |
| **Maintain envelope** | The W10b six-row WIN-block at the no-regression floor (`today × 0.98`, sonic floor where higher) — same six rows and floors as W3. The codec + string-block scanner are parse-loop-adjacent edits; CSSC CTZ in P2-D §4.4 carries an explicit **HARD blocking precondition** on exactly these six rows (REDRESS 89 regressed them 3-8%). Additionally the unicode direct-plane rows (`unicode_escapes/direct_to_struct`, `y_string_unicode/direct_to_struct`, `unicode_mixed/direct_to_struct`) must not regress — REDRESS 82's blocking rows become W4's no-regression guard (P2-D §3.5 #3). And because W4 rewires `match_string_at_quote_trusted_utf8`, a path the direct-to-struct projection also reaches, the three direct-GO rows — `citm_catalog`, `marine_ik`, `unicode_basic` `direct_to_struct A / GO` — are restated here as W4 maintain rows, each holding its `A / GO` outcome with no delta beyond noise, exactly as they are gated on W5. |
| **Revert protocol** | Per P2-E §7.1: the checkasm gate (slice S6) lands FIRST and blocks the wave — no consumer slice proceeds until S6 is green. If a NEON body fails checkasm parity, revert that body (S2-S5) and the JSON consumer falls back to the scalar reference S1. If the bench-row conditional gate fails for a row, that row stays NO-GO and the wave admits codec-contribution-only on it (records the falsified gate in REDRESS). If the W10b WIN-block regresses, revert the CSSC CTZ slice (P2-D §4.4) and the string-block widening producer-rewire. **Does not block W5** — W5's ASM kernels (EOR3, structural-bitmap) are independent of the codec; but W4 itself **blocks on W3** (the union substrate is P2-E's and P2-D §4.4's same-wave consumer). |
| **Same-wave consumer** | P2-E: one production consumer — the already-wired x4 JSON path `unescape_four_unicode_escapes` (`parse-that-regex/src/lib.rs:402`), re-bodied onto `escape_codec_hex_unit`; plus two scaffolds (CSS L4, TOML — compile-validated `#[cfg(test)]`, no production path, no falsifiability gate). P2-D §4: `scan_string_special_block_32`'s consumer is the existing `match_string_at_quote_trusted_utf8` (`parse-that-regex/src/lib.rs:162`) — wider blocks, not a new call site. The codec checkasm gate (`checkasm_escape_codec.rs`) and string-block checkasm gate (`checkasm_string_block.rs`) are both same-wave admission preconditions per P2-D §6.2.1. |

### W5 — aarch64 ASM substrate kernels (P2-D §5: EOR3 ladder, CSSC CTZ, structural-bitmap)

| Field | Specification |
|---|---|
| **Exit gate rows + thresholds** | String-dense parse_only Track 1 rows, standard-parity floor: `gsoc-2018` ≥ 41198 (the residual P2-A left open — `sonic 45318 / 1.10`); `github_events` ≥ 21360 / 1.10 = 19418 (today 14302); `random` ≥ 15166 / 1.10 = 13788 (today 9382). **Plus** the hot-leaf falsifiers (P2-D §2.4): `movemask_u8x16` self-time on `gsoc-2018` drops from 30.9% to ≤ 12%; the mask-+-CTZ pipeline (`movemask` + `<u16>::trailing_zeros` + `string_block_scan`) drops from 46.2% combined to ≤ 20% on `gsoc-2018`. The EOR3-ladder slice carries an additional `cargo asm` proof that `veor3q_u8` (not `pmull`) appears under `target-cpu=native` (P2-D §5.3.1). |
| **Maintain envelope** | The W10b six-row WIN-block — **the binding gate of this wave**, at the no-regression floor (`today × 0.98`, sonic floor where higher). P2-D §5.3.1 and §4.4 both name this exact six-row block as a HARD blocking precondition: the prefix-XOR hot body and the CSSC CTZ consumer are precisely the surfaces W10/W10b proved regress the WIN block 3-8% even correctness-green. No EOR3 body and no CTZ body ships unless `canada`, `citm_catalog`, `instruments`, `marine_ik`, `mesh`, `numbers` all hold their floor. The four typed-GO + three direct-GO rows must also show no delta beyond noise (the substrate kernels touch the parse loop, not the typed/direct projection — a delta there signals a cross-substrate leak). |
| **Revert protocol** | Per P2-D §5.3.1/§5.4: each kernel ships with a checkasm differential that lands before any wiring; checkasm failure blocks the wave. The EOR3 ladder is gated by the Lock 16 `FEAT_SHA3` host-capability predicate — the scalar shift-XOR ladder remains the unconditional fallback, so reverting the EOR3 body is a predicate-flip, not a parse-loop rollback. If the W10b WIN-block regresses, revert the offending kernel body and fall back to the scalar reference. **A full revert does not block any later wave** — W5 is the terminal behaviour wave; a revert routes the kernel back to S-P2/S-P3 under a fresh REDRESS material-differential. The structural-bitmap chain (P2-D §5.4) blocks on W3 (its only consumer is the union substrate's typed event cursor); if W3 reverted, W5's §5 structural-bitmap slice does not ship and `scan_structurals` stays `blocked_no_consumer`. |
| **Same-wave consumer** | Per P2-D §6.4: §5.3.1 EOR3 ladder → the union-substrate structural-bitmap producer (W3 substrate); §4.4 CSSC CTZ → the union-substrate string-mask consumer; §5.4 structural-bitmap chain → the union-substrate typed event cursor. Every kernel lands with its hot-path caller in the same commit; the four `blocked_no_consumer` primitives (`BULK_EMIT_COMPRESSED`, `FRAME_PUSH/POP_BOUNDED`, `FSM_DISPATCH_THREADED`) stay blocked — they unlock only with a CollapsedStage codegen consumer, out of scope for SK-V9. |

---

## §3 — The proof-only wave gates (W2)

W2 (P2-B retained class/event grammar + `ValueRef` cursor proof) is the
single proof-only wave. Per the S-P3 prompt and HANDOFF §3 — "Proof-only;
no `RESULTS.md` row movement at Alpha depth" — its gate is **compile-time
validity + cross-grammar witness, not Mbps**. A bench-row threshold on
W2 would be a category error: the proof has no production consumer, no
parse-loop edit, no Criterion surface.

### §3.1 — The W2 exit gate (compile-time, not measured)

W2 admits iff all of the following hold (P2-B §1.1, §1.4):

1. **The `EventGrammar` trait compiles**, is grammar-neutral by
   signature (no `match grammar` arm, no role enum), and `cargo check
   -p runtime` is green.
2. **Three witness instances compile** — the three
   `const _: fn() = _proof_compiles::<G>` lines for `JsonEventGrammar`,
   `SheetsEventGrammar` (the Lock-14 non-JSON witness), and `AnyGrammar`
   (the empty-alphabet default). The compiler refusing to emit any line
   is the proof's failure mode; all three emitting is the acceptance
   verdict.
3. **The cross-grammar witness is non-JSON** — at least one witness is
   CSS L4 or Sheets, compiling against the same trait with zero
   generic-crate source touched. Lock 14 is *exercised*, not claimed.
   CH2 falsifier: the `rg` audits at P2-B §3.3 — every `admits_fact` /
   `admits_class` match must be inside `event_grammar.rs`, a witness
   file, or the proof-test file; never inside generic substrate source.
4. **`ValueRef<'tape, 'src, G>` borrows pass `cargo check`** — no
   `'static` leak, no sidecar lifetime, no parser-owned cursor
   outliving the substrate. The negative compile test (a
   `ValueRef<'static, 'static, JsonEventGrammar>` against a borrowed
   tape) must be rejected by the borrow checker (P2-B §2.3).
5. **`cargo test -p runtime event_grammar` is green** — the type-level
   `_proof_compiles` lines run; no runtime parser code executes.

### §3.2 — The W2 maintain envelope

W2 moves no row, so the maintain envelope is **structural, not Mbps**:

- **No row in `skinny/RESULTS.md` moves.** The wave's verification
  surface is `cargo check` + `cargo test -p runtime event_grammar`;
  `cargo bench -p bbnf-bench` is explicitly NOT a verification surface
  (P2-B §1.1). A W2 commit that touches a generated `RESULTS.md` row
  has breached its own boundary and falsifies.
- **No edit to `generated.rs`, `scan.rs`, `parser.rs`, `view.rs`, or
  any codegen template** (P2-B §4.2 #3). The owner-files set is only
  new files under `runtime/src/tape/` and `runtime/src/grammars/*_witness/`
  plus a five-line `lib.rs` re-export behind `#[cfg(any(test, feature
  = "proof"))]` — the gate applied once at the parent `pub mod` site.
- **The witness files are excluded from the release library** — CH5
  falsifier: `rg 'event_grammar|event_grammar_witness'
  skinny/crates/bbnf-bench/` returns zero; the witnesses cannot be
  reached by any production caller or by `cargo bench`.

### §3.3 — The W2 revert protocol

Per P2-B §6.1, the proof decomposes into five slices (S1 trait, S2 JSON
witness, S3 Sheets/CSS witness, S4 `ValueRef` parameterisation, S5 cfg
gating + proof tests), each its own revert unit. A slice that overruns
its LOC cap reverts that slice only and re-plans. If S4 (the `K →
G: EventGrammar` rename) leaks to call sites, back out and reattempt
with a `pub type` alias instead of a parameter default. A failed proof
(any of the three `const _` lines refuses to compile) reverts the whole
wave — there is no partial proof.

**W2 does NOT block W3.** The proof is the *necessary* gate before a
future SK-V10+ tranche may reopen the SC-3 Tier A union-substrate
*migration* as a measured-row implementation. But the SK-V9 W3 union
event-model (P2-A) is itself the routed precursor REDRESS 92 named — it
implements the alternate event-model, not the rejected storage-only
swap. P3-B sequences W2 before W3 so the retained-grammar contract is
admitted before the class column lands; if W2 fails, W3's class-column
design must carry the contract proof inline or W3 is held. This is a
sequencing dependency, not a hard Mbps block — recorded so P3-B can
order it topologically (proof before consumer).

### §3.4 — Why W2 is not a paper-close

CH6 ANTI-PAPER-CLOSE asks: does the wave close on measurement, not a
future-phase promise? W2 closes on a **compiler verdict** — `cargo
check` emitting or refusing the three witness lines is a mechanically
checkable, falsifiable observation, not a promise. The same-wave-consumer
rule (CH4/CH6) is **silent** on W2 because, per P2-B §5.1, that rule
binds *substrates*, not *contracts*: the proof is a trait declaration
plus three witness `impl`s, none of which lands in the parse hot path.
W2's admission is therefore not blocked on furnishing a production
consumer that, by design, does not exist for a compile-only artefact.

---

## §4 — The conditional-admission rule for the unicode rows (W4)

This section is the load-bearing honesty point of P3-C. P2-E §6.4
established by PMU rederivation that **the unicode codec alone does NOT
close `unicode_escapes`, `y_string_unicode`, `unicode_mixed`, or
`gsoc-2018`**. The W4 falsifiability gate must encode the conditional
rule, not an aspirational "codec closes 4 rows."

### §4.1 — The rederived projection (P2-E §6.2, verbatim posture)

Projecting a 75% codec-class reduction (the NEON µop-count best case)
onto each row's measured `SK-V9-open` baseline:

| Row | Baseline Mbps | Codec c/B share | Projected Mbps (codec only) | sonic-strict | Threshold | Verdict (codec alone) |
|---|---:|---:|---:|---:|---:|---|
| `unicode_escapes` | 11239 | 36.2% | 15423 | 18132 | 16319 (×0.90 standard) | **NEAR-FAIL 94.5%** |
| `y_string_unicode` | 5457 | 40.5% | 7837 | 11814 | 8270 (×0.70 W4-precedent) | **NEAR-FAIL 94.8%** |
| `unicode_mixed` | 7276 | ~10% (folded in validator) | 7864 | 14515 | 12338 (×0.85 standard) | **FAIL 63.7%** |
| `gsoc-2018` | 21646 | ≈0% | 21646 (unchanged) | 45318 | 21430 (no-regression) | admitted **no-regression only** |

The codec is a strong contributor on two rows and a non-contributor on
two. **Zero rows admit on the codec alone at the standard / W4-precedent
slack.** This is the verbatim P2-E §6.4 verdict and the W4 gate is
written against it.

### §4.2 — The conditional-admission rule (the W4 exit gate)

W4 admits **per-row, on measurement**, under these four binding clauses:

1. **`unicode_escapes` / `y_string_unicode` — same-wave conditional.**
   Each admits iff its *measured* post-wave parse_only Track 1 Mbps
   clears its threshold (16319 / 8270). The §4.1 projections (15423 /
   7837) are flagged as the **expected-best-case bound** — a measured
   codec reduction of 70% or 80% rather than the projected 75% puts the
   row at ~90%-100% of threshold. The gate is the *measurement*, not
   the projection. If the measured row clears, it admits as `A / GO`;
   if it NEAR-MISSES, the row stays `S / NO-GO`, the codec records a
   measured contribution in REDRESS, and the residual routes to a
   future tranche. The codec is admitted as a *contributor* to these
   rows regardless of whether they cross the gate — the wave is not
   reverted for a NEAR-MISS, only the row's GO status is withheld.

2. **`unicode_mixed` — admission CONDITIONAL on same-wave pairing.**
   The codec touches only ~10% of this row's c/B (it folds into
   `validate_string_escape`). The codec intervention alone projects
   63.7% of threshold — a hard FAIL. Per P2-E §6.3 the row's admission
   is **conditional on a same-wave per-string-span scanner
   intervention**. The W4 wave therefore PAIRS the P2-E codec with the
   P2-D §4 string-block widening (`scan_string_special_block_32` +
   `validate_string_escape` TBL collapse, P2-D §2.2/§2.3) in the **same
   wave commit**. `unicode_mixed` admits iff the *combined* codec +
   string-block measured Mbps clears 12338. If the string-block
   widening does not land in the same wave, `unicode_mixed` stays
   `NO-GO` and W4 admits codec-contribution-only on this row — it must
   never be claimed closed by the codec.

3. **`gsoc-2018` — no-regression basis only.** The codec share is ≈0%;
   the row's load is the `movemask_u8x16` string-block scanner, a
   different primitive class. `gsoc-2018` admits in W4 iff its measured
   Mbps ≥ 21430 (`baseline − 1%`) — i.e. the codec must not regress it.
   Closing `gsoc-2018` is **out of scope for the codec** and routes to
   W5 (the P2-D §4 string-block widening + §5 movemask/CTZ work, exit
   gate ≥ 41198). A W4 gate that claims `gsoc-2018` closure is a
   paper-close and fails CH6.

4. **The wave-level rule.** W4 closes (admits as a wave) iff (a) every
   exit-gate row that admits does so on *measured* Mbps, (b) every row
   that NEAR-MISSES or FAILS has its honest verdict recorded in REDRESS
   with the measured contribution, and (c) the W10b WIN-block + the
   unicode direct-plane rows hold their no-regression floors. W4 does
   **not** revert wholesale on a per-row NEAR-MISS — the codec is a
   real, checkasm-verified contribution; it reverts only on a checkasm
   parity failure or a W10b WIN-block regression.

### §4.3 — Why the codec and string-block widening are one wave

The S-P3 prompt's same-wave-consumer requirement plus P2-E §6.3's
conditional rule force the pairing. If the codec and the string-block
widening were sequenced as two waves:

- The codec wave would close *zero* rows (per §4.1) — a wave that moves
  no row to `GO` and whose exit gate is "contribution recorded" is a
  paper-close under CH6.
- `unicode_mixed` would have no admission path at all in the codec wave
  (63.7% FAIL), and the string-block wave alone does not touch the
  codec-dominated `y_string_unicode`.

Pairing them is the only sequencing under which W4 has a non-vacuous
exit gate. P3-B must therefore manifest the codec + string-block
widening as **one wave**, not two. The CSSC CTZ slice (P2-D §4.4) and
the EOR3 / structural-bitmap kernels (P2-D §5) are separable into W5
because they target different rows (`gsoc-2018`, `github_events`,
`random`) and carry their own WIN-block blocking precondition.

### §4.4 — The conditional rule stated as a falsifier

The W4 gate is falsified — the wave is reverted, not merely a row
withheld — iff any of:

1. A NEON codec body or the `scan_string_special_block_32` body fails
   checkasm parity against its scalar reference (P2-E §7.3 / P2-D
   §6.2.1). The checkasm gate lands first and blocks all wiring.
2. Any W10b WIN-block row (`canada`, `citm_catalog`, `instruments`,
   `marine_ik`, `mesh`, `numbers`) drops below `today × 0.98`.
3. Any unicode direct-plane row (`unicode_escapes/direct_to_struct`,
   `y_string_unicode/direct_to_struct`, `unicode_mixed/direct_to_struct`)
   regresses — REDRESS 82's blocking rows are W4's no-regression guard.
4. The wave claims `gsoc-2018` or `unicode_mixed` closure without the
   measured combined Mbps clearing the threshold — a paper-close.

A per-row NEAR-MISS on `unicode_escapes` / `y_string_unicode` is **not**
a falsifier — it is the expected, honestly-projected outcome; the row
stays `NO-GO`, the contribution is recorded, and the wave still admits
the codec as a verified primitive. This is the conditional-admission
rule: the codec admits as a *primitive with a measured contribution*;
the *rows* admit only on measurement clearing their gate.

---

## §5 — Sources

Upstream artefacts cited:

1. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
   — S-P1 V6 convergence; the OLS `ns_per_byte ≈ 1.079·(q/B) +
   0.184·(n/B) + 0.051`, R²=0.371; the four uncloseable rows; the PMU
   table at `/tmp/skv9-xctrace-v3/pmu_rows.tsv`.
2. `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`
   — §4.1 must-improve thresholds (twitter/apache/gsoc/distinct_values/
   update_center); §4.2 W10b six-row binding maintain gate; §4.4
   falsifying observations; §5 per-slice revert protocol.
3. `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`
   — §1.1 proof shape; §3.3 Lock-14 `rg` audits; §5.1 same-wave-consumer
   formal disposition (rule binds substrates not contracts); §6.1
   five-slice revert.
4. `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`
   — §3.1/§3.2 per-row typed thresholds; §4.3 falsifiability gates;
   §2.0/§4.1 five-slice revert; §6 pre-block citations.
5. `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
   — §2 per-uncloseable-row diagnosis; §3.5 codec broadening (blocks on
   P2-A); §4 string-block widening + §4.4 CSSC CTZ W10b precondition;
   §5.3.1 EOR3 ladder + W10b maintain gate; §6.2.1 checkasm dispatch
   ownership; §6.4 orphan-rejection rule.
6. `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`
   — §6.1/§6.2 PMU-rederived baseline + projection; §6.3 three slack
   rules + conditional rule; §6.4 honest verdict (zero rows admit on
   codec alone); §7.1 eleven-slice revert; §4 same-wave consumer +
   scaffold distinction.
7. `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md`
   — §1 sonic-rs strict same-run anchor; §2 per-corpus competitive
   position + Mbps; §7 the >SOTA close criterion + intervention
   dependency graph (`I ← P2-A ← P2-B`, `II ← P2-E`, `III ← P2-D ←
   P2-A`).
8. `restart/skinny/tranches/sk-v9/SPEC.md` §0 — SK-V9 close condition +
   goalset; §2 wave manifest (W0-W5 placeholders).
9. `skinny/RESULTS.md` — the 38-row `SK-V9-open` report under
   `sk-v9-open:criterion-fnv64-cd1673844eeea12f`; per-row parse_only
   Track 1 Mbps; the four typed-GO rows (twitter/update_center/mesh/
   marine_ik `real_typed_struct A/GO`); the three direct-GO rows
   (citm_catalog/marine_ik/unicode_basic `direct_to_struct A/GO`).
10. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` — §2 P3-C scope;
    §3 CH1/CH4/CH6 lens registry; §8 bbnf-lang axes (W0 baseline first,
    same-wave consumer per kernel, falsifiability gates same-row).
11. `restart/prompts/ORCHESTRATOR.md` §8 — profile-first non-negotiable;
    no hypothesis transfer.
12. `skinny/REDRESS.md` — Items 82 (W4 single-quartet classifier), 88
    (PMULL prefix-XOR), 89 (CSSC CTZ bulk consumer), 91 (Apache/CITM
    source/product parity), 92 (W3 event-model fit-gate) — the
    pre-blocked routes each wave's revert protocol must not re-open.

## §0 — V2 fold

V2 fold: F-AUX surgical touch-up per S-P3 V1 CHALLENGE.
