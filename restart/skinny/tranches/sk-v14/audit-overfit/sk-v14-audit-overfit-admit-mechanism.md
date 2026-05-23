# SK-V14 S-P0 Axis A2 — Admit-Mechanism Integrity

## §0 — Disposition summary

Pass criterion (from `PASS-0-OVERFIT-AUDIT.md §Scope` row A2, verbatim):

> No admit lands by gate-relabel; every admit cites a parser/codegen/SIMD
> source change; comparator is strict-vs-strict on the same plane.

- Findings: **CRITICAL = 4, HIGH = 3, MED = 1, LOW = 1** (total 9 findings).
- Verdict: **FAIL** (consistent with HANDOFF §3 audit-zero baseline; no
  regression and no new admit-mechanism failure has slipped in since the
  SK-V13 audit pack landed).
- Confirms / extends SK-V13 audit pack: **YES — confirms in full**;
  delta is one *negative* observation (no new admits added between
  audit close + SK-V14 contract close at `00181742e`) and one *positive*
  scope-extension observation (the W15.1 UpdateCenter typed admit at
  `7ec4a474c` — outside §A2 scope, which is bound to W14.1-5 per the
  dispatch — DID touch real codegen + generated parser; it is the only
  post-audit admit and it is mechanically clean by the A2 lens, though
  it remains comparator-misbound by the A1/A6 lens, captured under R1).
- New findings (not in SK-V13 audit pack): **2** — (i) the ROLLING-SOTA-DELTA
  ledger has had no admit additions across the 17 SK-V14 alpha-doc commits
  (`496a81417` → `00181742e`), confirming the SK-V14 baseline is
  unchanged-since-audit; (ii) the comparator-misbinding pattern is
  *uniform* across all three JSON planes — a single `sonic_rs::from_slice`
  binding pretends to serve as strict comparator for parse_only, direct,
  and typed planes simultaneously (a structural defect, not a per-row
  defect; PRUNE-1 reverts the symptoms, R1 fixes the cause).

## §1 — Methodology

Per the §3 executable-verification mandate, every claim below is
backed by a shell command actually run + its output quoted verbatim.

### Commands executed

1. `git show 5d5490f08 --stat` — W14.1 numbers parse_only.
2. `git show c7f3e42a5 --stat` — W14.2 citm_catalog parse_only.
3. `git show 37a791d42 --stat` — W14.3 canada parse_only.
4. `git show 71508ea93 --stat` — W14.4 marine_ik parse_only.
5. `git show 93eb60182 --stat` — W14.5 mesh parse_only.
6. `for sha in 5d5490f08 c7f3e42a5 37a791d42 71508ea93 93eb60182;
   do git show --name-only $sha | grep -E '(generated\.rs|generated_json|
   parse_direct|json_templates|grammars/json/)'; done`
   — empty for all 5 (no parser / codegen / grammar files touched).
7. `git grep -n 'per_iter_equality\|equality_oracle\|sonic_rs::from_slice'
   skinny/` — 21 hits enumerated below.
8. `git grep -nE 'b\.iter.*assert|assert_eq.*sonic|assert.*parity.*\.iter'
   skinny/crates/bbnf-bench/benches/` — empty (no per-iter equality
   inside any `b.iter` closure).
9. `git log --oneline restart/skinny/ROLLING-SOTA-DELTA.md` — last admit
   commit is `7ec4a474c` (W15.1, 2026-05-22 14:28), before SK-V14
   alpha-bracket opens at `496a81417`; no admit row has landed since.
10. Read `skinny/crates/bbnf-bench/benches/json_parity.rs:1-120`
    — comparator binding region quoted verbatim below.

### Files audited

- `restart/skinny/tranches/sk-v13/audit-overfit/sk-v13-audit-overfit-json-parse-only.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v2-json-validation.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v6-comparator-integrity.md`
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` §0.4, §1.3, §3
- `restart/skinny/tranches/sk-v14/HANDOFF.md` §3, §7
- `skinny/crates/bbnf-bench/benches/json_parity.rs` (528 lines, comparator surface)
- W14.1-5 commit metadata via `git show --stat`.

### Verification budget

Roughly 18 min wall consumed before write; 7 min remain.

## §2 — Per-finding ledger

### §2.1 — W14.1-5 source-diff verification table (gate-relabel-only confirmed)

| Wave | Corpus | SHA | Insertions / Deletions | Behavior path touched? | Files cited |
| ---- | ------ | --- | ---------------------- | ---------------------- | ----------- |
| W14.1 | numbers      | `5d5490f08` | +1052 / -176 | **NO** | `ROLLING-SOTA-DELTA.md`, `numbers-parse-facts.json`, `redress.md`, `skv13-W14.1-json-parse-only.json`, `REDRESS.md`, `RESULTS.md`, `gate.rs`, `lock14_baseline.rs`, `report.rs`, `xtask/main.rs` (10 files; 0 parser, 0 codegen, 0 grammar) |
| W14.2 | citm_catalog | `c7f3e42a5` | +633 / -162 | **NO** | `ROLLING-SOTA-DELTA.md`, `citm-catalog-parse-facts.json`, `redress.md`, `skv13-W14.2-json-parse-only.json`, `REDRESS.md`, `RESULTS.md`, `gate.rs`, `lock14_baseline.rs`, `report.rs` (9 files; 0 parser, 0 codegen, 0 grammar) |
| W14.3 | canada       | `37a791d42` | +290 / -55 | **NO** | `ROLLING-SOTA-DELTA.md`, `canada-parse-facts.json`, `redress.md`, `skv13-W14.3-json-parse-only.json`, `REDRESS.md`, `RESULTS.md`, `gate.rs`, `report.rs` (8 files; 0 parser, 0 codegen, 0 grammar) |
| W14.4 | marine_ik    | `71508ea93` | +307 / -51 | **NO** | `ROLLING-SOTA-DELTA.md`, `marine-ik-parse-facts.json`, `redress.md`, `skv13-W14.4-json-parse-only.json`, `REDRESS.md`, `RESULTS.md`, `gate.rs`, `report.rs` (8 files; 0 parser, 0 codegen, 0 grammar) |
| W14.5 | mesh         | `93eb60182` | +313 / -52 | **NO** | `ROLLING-SOTA-DELTA.md`, `mesh-parse-facts.json`, `redress.md`, `skv13-W14.5-json-parse-only.json`, `REDRESS.md`, `RESULTS.md`, `gate.rs`, `report.rs` (8 files; 0 parser, 0 codegen, 0 grammar) |

Source-diff pattern grep:

```
$ for sha in 5d5490f08 c7f3e42a5 37a791d42 71508ea93 93eb60182; do
    git show --name-only $sha | grep -E '(generated\.rs|generated_json|parse_direct|json_templates|grammars/json/)'
  done
(empty output — zero hits for all five commits)
```

The `gate.rs`, `lock14_baseline.rs`, `report.rs`, `xtask/main.rs` files
touched by W14.1-5 are **gate / report / lock14 infrastructure**: they
add a JSON schema, an admission-spec table, validation tests, and
report-emission glue so a pre-existing measurement can be re-stamped
from `S/NO-GO` to `A/GO` without re-measuring or re-parsing. None of
them participates in the timed bench loop.

### §2.2 — Comparator binding (verbatim `json_parity.rs:87-102`)

```rust
group.bench_function("sonic_rs_anchor", |b| {
    b.iter(|| {
        let value = sonic_rs::from_slice::<sonic_rs::Value>(black_box(&fixture.bytes)).unwrap();
        black_box(value);
    });
});
write_competitor_row(
    host,
    fixture,
    "sonic_rs_anchor",
    "sonic-rs",
    "0.5.8",
    "eager_typed",
    measurement_time_s,
    sample_size as u32,
);
```

The competitor row literally tags the work mode as `"eager_typed"`
(line 99). The bench framework itself records that this lane is
**eager-typed DOM deserialisation**, not a parse_only / structural-skip
operation; the row labels in `RESULTS.md` and `ROLLING-SOTA-DELTA.md`
nevertheless route this lane as the strict comparator for the
`parse_only` plane, the `direct_to_struct` plane, AND the
`real_typed_struct` plane (per `v6 §1`). One binding, three planes,
none of them work-equivalent — a single structural defect that
PRUNE-1 reverts at the row level and R1 closes at the harness level.

### §2.3 — sonic_rs::from_slice fan-out (campaign-wide; 21 hits in skinny/)

```
skinny/crates/bbnf-bench/benches/json_parity.rs:89:            let value = sonic_rs::from_slice::<sonic_rs::Value>(black_box(&fixture.bytes)).unwrap();
skinny/crates/bbnf-bench/src/bin/gate.rs:4490:            let value = sonic_rs::from_slice::<sonic_rs::Value>(&bytes)?;
skinny/crates/bbnf-bench/src/direct_struct.rs:428:    sonic_rs::from_slice(bytes).map_err(|error| DirectStructError::Sonic(error.to_string()))
skinny/crates/bbnf-bench/src/metadata.rs:483:        ("sonic-rs", _) => "sonic_rs::from_slice::<T>",
skinny/crates/bbnf-bench/src/real_typed_struct.rs:695: sonic_rs::from_slice::<TwitterSearch<'a>>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:698: sonic_rs::from_slice::<ApacheBuilds<'a>>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:701: sonic_rs::from_slice::<CitmCatalog<'a>>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:704: sonic_rs::from_slice::<Vec<GithubEvent<'a>>>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:707: sonic_rs::from_slice::<UpdateCenter<'a>>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:710: sonic_rs::from_slice::<Mesh>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:713: sonic_rs::from_slice::<MarineIk>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:716: sonic_rs::from_slice::<InstrumentsDocument<'a>>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:719: sonic_rs::from_slice::<Vec<f64>>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:723: sonic_rs::from_slice::<Vec<UnicodeBasicRecord<'a>>>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:727: sonic_rs::from_slice::<RandomDocument<'a>>(bytes) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:1361: sonic_rs::from_slice::<Vec<W5ArrayEvent<'_>>>(input) ...
skinny/crates/bbnf-bench/src/real_typed_struct.rs:1379: sonic_rs::from_slice::<W5MapMetricEntries<'_>>(input) ...
```

Two observations cross-check `v6 §1`:

1. The **parse_only plane shares its comparator** with the direct +
   typed planes via the single `sonic_rs_anchor` lane (line 89). The
   plane label is decoupled from the work the comparator actually does.
2. The **typed plane** already has per-corpus typed bindings
   (`real_typed_struct.rs:695-727`) that DO match Track 1's per-corpus
   typed materialisation work — those 11 hits are work-equivalent.
   The remaining typed admits are still reverted under PRUNE-1 because
   the *anchor* lane (the comparator the admit-gate measures against)
   is still the generic-Value binding at `json_parity.rs:89`, not the
   per-corpus typed binding at `real_typed_struct.rs:695-727`. Until
   the anchor lane is rebound (R1), the per-corpus bindings exist as
   parity-oracle adornment, not as the strict comparator.

### §2.4 — Per-iteration equality oracle (absent)

```
$ git grep -nE 'b\.iter.*assert|assert_eq.*sonic|assert.*parity.*\.iter' \
    skinny/crates/bbnf-bench/benches/
(empty)

$ git grep -n 'per_iter_equality\|equality_oracle' skinny/
(no hits — neither identifier exists in source today)
```

`json_parity.rs:17-26` runs `assert_parity` + `assert_direct_struct_parity`
+ `assert_real_typed_parity` ONCE per fixture at the top of
`bench_json_parity`, **before** the timed `run_fixture` call (line 27).
Inside every `b.iter(|| { … })` closure (lines 44-46, 66-68, 88-91,
105-107, etc.) the only operations are parse-and-`black_box`; there
is no equality check, no checksum compare, no parity assertion. The
amendment's per-iter strict-equality requirement is unmet system-wide;
not a row-level defect. R2 (per-iter equality oracle) is the closure.

### §2.5 — Post-audit drift check (negative finding — no new admits)

```
$ git log --oneline restart/skinny/ROLLING-SOTA-DELTA.md | head -5
7ec4a474c feat(sk-v13-waveW15.1): admit UpdateCenter typed plugin fast path
93eb60182 feat(sk-v13-waveW14.5): admit Mesh parse-only row
71508ea93 feat(sk-v13-waveW14.4): admit Marine IK parse-only row
37a791d42 feat(sk-v13-waveW14.3): admit Canada parse-only row
c7f3e42a5 feat(sk-v13-waveW14.2): admit CITM catalog parse-only row
```

Between `7ec4a474c` (last admit; W15.1 UpdateCenter typed, 2026-05-22
14:28) and the current head, every commit is a documentation /
synthesis commit under the SK-V14 alpha + audit-overfit brackets
(`496a81417` … `12ff0744e`); the audit baseline holds.

W15.1 is outside §A2's W14.1-5 scope, but the dispatch context asks
to confirm no NEW admit slipped in between the audit and SK-V14 close.
W15.1 *did* land after the V2-V6 validation pack but before the audit
pack itself was committed (`12ff0744e` is the audit dispatch). It IS
included in HANDOFF §3 ("11 comparator-misbound typed to revert under
R1"); its source diff DID touch real codegen
(`skinny/crates/codegen/src/json_typed_direct.rs` +157) and a real
generated parser (`skinny/crates/bbnf-bench/src/generated_real_typed.rs`
+125). By the §A2 lens (parser/codegen/SIMD source change), W15.1 is
**mechanically clean**; by the A1/A6 comparator-binding lens it is
still misbound and remains in the PRUNE-1 sweep under R1.

### §2.6 — Findings ledger

| # | Severity | Finding | Citation | Status |
| - | -------- | ------- | -------- | ------ |
| F1 | CRITICAL | W14.1 numbers parse_only admit is gate-relabel only; commit `5d5490f08` (+1052/-176 across 10 files) touches `gate.rs`, `lock14_baseline.rs`, `report.rs`, `xtask/main.rs`, plus 6 doc/research files; zero parser / codegen / grammar diff. | `git show 5d5490f08 --stat`; `skinny/crates/bbnf-bench/src/bin/gate.rs`, `…/lock14_baseline.rs`, `…/report.rs`, `skinny/xtask/src/main.rs`; v2 §1 row 1 | **CONFIRMS V13** (v2 §1 W14.1) |
| F2 | CRITICAL | W14.2 citm_catalog parse_only admit is gate-relabel only; commit `c7f3e42a5` (+633/-162 across 9 files) touches `gate.rs`, `lock14_baseline.rs`, `report.rs`, plus 6 doc/research files; zero parser / codegen / grammar diff. | `git show c7f3e42a5 --stat`; v2 §1 row 2 | **CONFIRMS V13** |
| F3 | CRITICAL | W14.3 canada parse_only admit is gate-relabel only; commit `37a791d42` (+290/-55 across 8 files) touches `gate.rs`, `report.rs`, plus 6 doc/research files; zero parser / codegen / grammar diff. | `git show 37a791d42 --stat`; v2 §1 row 3 | **CONFIRMS V13** |
| F4 | CRITICAL | W14.4 marine_ik parse_only admit is gate-relabel only; commit `71508ea93` (+307/-51 across 8 files) touches `gate.rs`, `report.rs`, plus 6 doc/research files; zero parser / codegen / grammar diff. | `git show 71508ea93 --stat`; v2 §1 row 4 | **CONFIRMS V13** |
| F5 | HIGH | W14.5 mesh parse_only admit is gate-relabel only; commit `93eb60182` (+313/-52 across 8 files) touches `gate.rs`, `report.rs`, plus 6 doc/research files; zero parser / codegen / grammar diff. (Severity HIGH rather than CRITICAL because mesh carries the smallest margin — 1228 Mbps, 1.10× — and thus the smallest tranche-damage surface; mechanism is identical to W14.1-4.) | `git show 93eb60182 --stat`; v2 §1 row 5 | **CONFIRMS V13** |
| F6 | HIGH | Comparator binding at `benches/json_parity.rs:87-102` calls `sonic_rs::from_slice::<sonic_rs::Value>` and self-labels the lane as `eager_typed` (line 99) — work-asymmetric with parse_only (which should structural-skip), with direct (which should per-corpus struct-deserialise), AND with typed (which should per-corpus typed-deserialise). One binding routed as strict comparator for three planes. | `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102`; v6 §1 + §3; HANDOFF §7 row "comparator-misbound" | **CONFIRMS V13** (v6 §1 rows 1-4) |
| F7 | HIGH | No per-iteration equality oracle anywhere in the bench harness; all four `assert_*parity` calls fire once per fixture at `json_parity.rs:17-26`, BEFORE the timed `run_fixture` call at line 27. Inside every `b.iter` closure, the sole operations are parse + `black_box`. Grep `per_iter_equality`/`equality_oracle` returns zero hits in `skinny/`. Speed margins are measured without per-iter correctness witness. | `skinny/crates/bbnf-bench/benches/json_parity.rs:17-27, 44-46, 66-68, 88-91`; v2 §3.3, §4.3; v6 §4; HANDOFF §7 row "R2" | **CONFIRMS V13** |
| F8 | MED | Single `sonic_rs_anchor` lane (`json_parity.rs:87-102`) is registered as the strict comparator for all three JSON planes simultaneously. The per-corpus typed bindings in `real_typed_struct.rs:695-727` exist but are wired into the *parity assertion* (startup-only), not the *anchor row* (admit-gate). The structural defect is the harness, not the per-row admit; PRUNE-1 reverts symptoms, R1 closes the cause. | `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102`, `…/src/real_typed_struct.rs:695-727`, `…/src/metadata.rs:483`; SYNTHESIS §3 R1 | **NEW** (extension of v6 §1 — observes the structural cause, not just the per-row symptoms) |
| F9 | LOW | No drift between SK-V13 audit close + SK-V14 contract close (`00181742e`): `ROLLING-SOTA-DELTA.md` has not received a single admit-row commit since `7ec4a474c` (W15.1, 2026-05-22 14:28). All 17 commits since are documentation. Negative finding — confirms baseline. | `git log --oneline restart/skinny/ROLLING-SOTA-DELTA.md` (last admit `7ec4a474c`); `git log --oneline restart/skinny/tranches/sk-v14/` (17 doc commits) | **NEW** (negative; confirms freeze) |

## §3 — Pass criterion verdict

The pass criterion (verbatim) is:

> No admit lands by gate-relabel; every admit cites a
> parser/codegen/SIMD source change; comparator is strict-vs-strict
> on the same plane.

**Verdict: FAIL.**

- **"No admit lands by gate-relabel":** violated by all 5 W14.1-5 parse_only
  admits (F1–F5). Source diffs are 100% gate / report / lock14 / doc /
  research; zero parser, codegen, grammar, or SIMD bytes touched.
- **"Every admit cites a parser/codegen/SIMD source change":** violated
  by W14.1-5 (F1–F5). Tangentially, W15.1 UpdateCenter typed at
  `7ec4a474c` *does* meet this clause for the typed plane (codegen
  +157, generated +125) — outside §A2's W14.1-5 scope but worth marking
  as a positive existence proof.
- **"Comparator is strict-vs-strict on the same plane":** violated by
  the single `sonic_rs_anchor` lane (F6, F8). One eager-typed binding
  carries three planes' admit-gates; the harness self-labels the work
  mode as `eager_typed` while RESULTS routes it as `parse_only`,
  `direct_to_struct`, and `real_typed_struct` strict-comparator. The
  per-iter equality clause (F7) is also unmet system-wide.

Per the dispatch §3 + the `PASS-0-OVERFIT-AUDIT.md §Failure mode` clause,
**the campaign cannot dispatch new behavior waves on top of these
admits.** The SK-V14 contract already binds this verdict: HANDOFF §3
declares the audit-zero baseline as 0/17 across all three JSON planes,
SYNTHESIS §3 sequences C-5 (PRUNE-1) before any new-admit wave, and
the SK-V14 wave program is held behind G-S-P0-CONVERGED per HANDOFF
§4.

## §4 — Recommended prune actions

A2's findings map onto the SK-V14 prune slate cleanly; no new
prune-wave proposal is required.

| Finding | Closure mechanism | Wave |
| ------- | ----------------- | ---- |
| F1 (W14.1 numbers) | revert admit in `RESULTS.md` + `ROLLING-SOTA-DELTA.md`; REDRESS row cites `v2 §1 row 1`; preserve gate/report scaffold for re-use under R8 | **PRUNE-1** (C-5) |
| F2 (W14.2 citm_catalog) | same; REDRESS cites `v2 §1 row 2` | **PRUNE-1** (C-5) |
| F3 (W14.3 canada) | same; REDRESS cites `v2 §1 row 3` | **PRUNE-1** (C-5) |
| F4 (W14.4 marine_ik) | same; REDRESS cites `v2 §1 row 4` | **PRUNE-1** (C-5) |
| F5 (W14.5 mesh) | same; REDRESS cites `v2 §1 row 5` | **PRUNE-1** (C-5) |
| F6 (comparator misbinding) | rebind `sonic_rs_anchor` to three plane-correct strict comparators: parse_only → sonic-rs Skipper-class structural-skip; direct → per-corpus strict struct deser; typed → per-corpus typed deser. No row may admit until its plane's comparator is strict-vs-strict. | **C-2 / R1** (NEW, ~600–1.08k LOC; HANDOFF §7 R1) |
| F7 (per-iter oracle absent) | add equality-pass column emitted per `b.iter` invocation inside the timing region; `xtask gate-json` rejects rows whose column is empty | **C-2 / R2** (paired with R1) |
| F8 (single-lane fan-out — structural cause) | the harness becomes the prevention surface for this entire pattern: one `comparator_plane` column per row (already proposed in SYNTHESIS §3 metadata-schema delta), with `xtask gate-json` rejecting any row whose comparator does work asymmetric to Track 1 | **C-2 / R1** (same wave; harness column lives at `bbnf-bench/src/metadata.rs`) |
| F9 (no post-audit drift) | nothing to do; this is the negative confirmation the audit baseline still holds and SK-V14 may proceed into S-P0 synthesis | (none) |

Cross-reference to SK-V14 SYNTHESIS §3 candidates:

- **C-2** (R1 + R2 — comparator rebind + per-iter equality oracle):
  binds F6 + F7 + F8; ~600–1.08k LOC; HIGH-risk per the SYNTHESIS
  estimate.
- **C-5** (R3 PRUNE-1 + PRUNE-2 — clean revert of fake admits):
  binds F1–F5; ~250–500 LOC, deletion-heavy; MED-LOW risk.
- **R6 / R7 / R8** are the re-admit waves that consume C-2 + C-5; they
  cannot dispatch until C-2 + C-5 land. R8 in particular requires a
  distinct `parse_only` code path in `generated_json` (no full-tape
  build) before parse_only re-admits are even attempted.

Execution order per HANDOFF §4 step 9: **PRUNE waves (C-5 → C-1 →
C-2 → C-3 → C-4) FIRST**; new-admit waves (R6 + R7 + R8) only after
PRUNE converges and `ROLLING-SOTA-DELTA.md` is restated to the §3
honest baseline.
