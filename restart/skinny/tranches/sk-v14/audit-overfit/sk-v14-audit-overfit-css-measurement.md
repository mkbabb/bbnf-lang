# SK-V14 S-P0 Axis A1 — Measurement Integrity

## §0 — Disposition summary

Pass criterion (verbatim from `PASS-0-OVERFIT-AUDIT.md §Scope` row A1):
> Every ADMITTED row in the prior tranche's ROLLING-SOTA-DELTA has (a) representative corpus ≥ 1 KB, (b) per-row distinct measurement, (c) plausible Mbps relative to byte-throughput ceiling.

- Findings: CRITICAL=4, HIGH=2, MED=2, LOW=0
- **Verdict: FAIL** on every leg (a) (b) (c); the SK-V14 starting baseline at HEAD `12ff0744e` reproduces the SK-V13 close-state pathologies verbatim.
- Confirms SK-V13 audit pack (v1 §1–6; v2 §1–4; v6 §1–3) with **zero delta** in the audited surface — every ROLLING-SOTA-DELTA byte and every bench-harness comparator line stand unchanged since the audit pack landed at commit `2e08f0c7c` on 2026-05-22 14:48 EDT.
- New findings (not in SK-V13 audit pack): 0. Measurement-integrity audit reduces to a confirmation pass under SK-V14's no-implementation-yet posture; the SK-V13 findings hold byte-identically.

The audit therefore validates the SK-V14 SYNTHESIS §3 C-5 binding (PRUNE-1 + PRUNE-2 revert) and the §0.2 audit-zero honest delta (CSS L4 0/24, parse_only 0/17, direct 0/17, typed 0/17).

---

## §1 — Methodology

Six executable verification swathes, each reproduced inline. All counts and bytes are quoted from live tool output, not cited from prior findings.

### §1.1 SOTA-delta motion since the audit pack

```
$ git log --format="%h %ai %s" 2e08f0c7c -1
2e08f0c7c 2026-05-22 14:48:22 -0400 docs(sk-v13-audit-overfit): css measurement and corpus integrity

$ git log 2e08f0c7c..HEAD --format="%h %ai %s" -- \
    restart/skinny/ROLLING-SOTA-DELTA.md skinny/RESULTS.md
[empty]

$ git log --since="2026-05-22 14:48" --oneline | wc -l
31

$ git log --since="2026-05-22 14:48" --oneline | grep -E "^[0-9a-f]+ feat" | wc -l
0
```

Zero `feat()` commits since the audit pack landed; the 31 post-audit commits are exhausted by `docs(sk-v13-audit-overfit-validation)` (v1–v6) and `docs(sk-v14-alpha*)` Pass-Alpha bracket activity. The ROLLING-SOTA-DELTA file is byte-identical to its `2e08f0c7c` predecessor (zero hunks listed).

### §1.2 ADMITTED row census per plane

```
$ grep "ADMITTED" restart/skinny/ROLLING-SOTA-DELTA.md | grep "parse_only"     | wc -l
5
$ grep "ADMITTED" restart/skinny/ROLLING-SOTA-DELTA.md | grep "direct_to_struct"| wc -l
29
$ grep "ADMITTED" restart/skinny/ROLLING-SOTA-DELTA.md | grep "real_typed_struct"| wc -l
11
$ grep "ADMITTED" restart/skinny/ROLLING-SOTA-DELTA.md | grep "css_l4"          | wc -l
24
$ grep -c "ADMITTED" restart/skinny/ROLLING-SOTA-DELTA.md
45
```

The 29-strong `direct_to_struct` count subsumes 5 JSON direct rows plus 24 CSS L4 rows (CSS uses the same `direct_to_struct` plane label). Net per-plane: 5 JSON parse_only + 5 JSON direct + 11 JSON typed + 24 CSS L4 = 45 ADMITTED. Cross-checks the SK-V14 SYNTHESIS §0.2 honest-delta table's "to be reverted" arithmetic (5 + 6 + 11 + 24 = 46) — the 1-row discrepancy reflects v2's "ADMIT-HOLDS" verdict for direct rows (the comparator binds the same plane on direct/typed, even if not strict-skip), whereas SYNTHESIS §0.2 reverts the full 6 direct rows the addendum's "strict" gloss would invalidate.

### §1.3 CSS corpus inventory and byte counts

```
$ find restart/skinny/tranches/sk-v12/research/w1b \
       restart/skinny/tranches/sk-v13/research -name "*.css" \
       -exec wc -c {} \;
187 restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css
162 restart/skinny/tranches/sk-v13/research/w10.2/css_l4_vendor_and_custom_atrules.css
351 restart/skinny/tranches/sk-v13/research/w10.3/css_l4_nested_layout.css
357 restart/skinny/tranches/sk-v13/research/w4/css_l4_visual_functions.css
305 restart/skinny/tranches/sk-v13/research/w3/css_l4_declaration_values_extended.css
117 restart/skinny/tranches/sk-v13/research/w2/css_l4_stylesheet_and_selectors.css
 85 restart/skinny/tranches/sk-v13/research/w10.1/css_l4_at_rules_and_media.css

$ ls skinny/corpora/ 2>&1 | grep -i css || echo "absent"
absent
```

Seven distinct CSS fixtures, all in the 85-357 byte range. The criterion `(a) representative corpus ≥ 1 KB` fails on every one of the 24 CSS L4 ADMITTED rows: the largest fixture (357 B, W4) is less than 36 % of the 1 KB floor; the smallest (85 B, W10.1) is 8 %. The expected production-corpus directory `skinny/corpora/css-l4-sk-v13/` does not exist; SK-V14 SYNTHESIS C-3 calls for `skinny/corpora/css-l4-sk-v14/` (~960 KB Bootstrap + Tailwind + Material + Animate), which the present tree likewise lacks.

### §1.4 Per-parse nanosecond plausibility (byte-throughput ceiling)

Formula `elapsed_ns = bytes × 8000 / Mbps` applied to the seven distinct measurement clusters in the CSS L4 delta:

| Grouped row | Bytes | T1 Mbps | LC Mbps | T1 ns/parse | LC ns/parse | Ratio |
|---|---:|---:|---:|---:|---:|---:|
| W1b declaration_values | 187 | 434.13 | 169.23 | 3 446 | 8 840 | 2.6× |
| W3 declarations etc. (×5 sub-rows) | 305 | 265.72 | 55.91 | 9 183 | 43 642 | 4.8× |
| W2 stylesheet + selectors (×5) | 117 | 26 894.88 | 596.05 | **34.80** | 1 570 | **45.1×** |
| W10.1 at_rules + media (×2) | 85 | 21 584.64 | 254.22 | **31.50** | 2 675 | **84.9×** |
| W10.2 vendor + custom (×2) | 162 | 34 635.22 | 278.74 | **37.42** | 4 649 | **124.3×** |
| W4 visual functions (×4) | 357 | 225.89 | 115.53 | 12 643 | 24 721 | 2.0× |
| W10.3 nested layout (×5) | 351 | 52 233.54 | 422.16 | **53.76** | 6 652 | **123.7×** |

Four of the seven clusters (W2, W10.1, W10.2, W10.3 — 16 of the 24 admitted rows) parse in 31-54 ns per cold invocation. That is **below typical Criterion harness overhead** (function-call + black_box + timer sample) for cold per-parse measurement; at sub-LLC latencies the Mbps figure ceases to track parser work and instead tracks measurement noise. The W10.2 124.3× ratio over lightningcss on a 162-byte fixture is the strongest single overfit signal, with W10.3 (123.7× on 351 B) a close second; both correspond to fixtures specifically engineered to minimum viable size, not representative load.

### §1.5 Identical-number clusters across rolling-delta rows

```
$ awk -F'|' '/ADMITTED/ && /css_l4/ {print $4, $5, $6}' \
      restart/skinny/ROLLING-SOTA-DELTA.md | sort | uniq -c | sort -rn
   5  52233.54  422.16  51811.38
   5  26894.88  596.05  26298.83
   5  265.72   55.91    209.81
   4  225.89   115.53   110.37
   2  34635.22 278.74   34356.48
   2  21584.64 254.22   21330.42
   1  434.13   169.23   264.90
```

The 24 CSS L4 admitted rows compress to seven distinct (T1, lightningcss, margin) triples. Five clusters share their numbers across 2-5 rolling-delta rows each, confirming v1 §2's "grouped measurement, multi-feature rolling delta row" pattern: per-row distinct measurement (criterion b) FAILS on 17 of 24 CSS L4 rows. The grouping is documented in v1 §2 as intentional per SPEC §14, but the rolling delta does not surface the grouping to the consumer — every row presents as if independently measured.

### §1.6 JSON parse_only comparator binding (still misbound)

```
$ wc -l skinny/crates/bbnf-bench/benches/json_parity.rs
528 skinny/crates/bbnf-bench/benches/json_parity.rs

$ sed -n '43,53p;87,102p' skinny/crates/bbnf-bench/benches/json_parity.rs
    group.bench_function("track1_generated", |b| {
        b.iter(|| {
            let root = runtime::generated_json::parse(black_box(input)).unwrap();
            black_box(root);
        });
    });
    let track1_payload = track1_payload_counters(input);
    write_row(
        host,
        fixture,
        "track1_generated",
    …
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
        …
```

The `track1_generated` bench at line 43-48 calls `runtime::generated_json::parse` (the unconditional full-tape builder per v2 §2; cross-confirmed by `parity.rs:79`, `materialization.rs:104`, `bin/xctrace_probe.rs:116`). The comparator at line 89 is `sonic_rs::from_slice::<sonic_rs::Value>` (eager-typed DOM construction) and is recorded with workload-tag `eager_typed` at line 99 — yet the rolling-delta row plane is `parse_only`. Same misbinding as v2 §3, byte-identical to the line-87-89 fragment quoted in the SK-V14 SYNTHESIS §0.4 P-1 pre-block. The throughput-plausibility criterion (c) is moot for these rows because the comparator measures different work than the row title claims.

---

## §2 — Per-finding ledger

| Severity | Finding | Citation | Status |
|---|---|---|---|
| CRITICAL | All 24 CSS L4 ADMITTED rows fail criterion (a) corpus ≥ 1 KB; fixtures are 85-357 B research fragments. | `find … -name "*.css" -exec wc -c \;` §1.3; `ROLLING-SOTA-DELTA.md` 24 CSS rows; `restart/skinny/tranches/sk-v13/audit-overfit/sk-v13-audit-overfit-css-measurement.md:14-22` | CONFIRMS V13 (byte-identical) |
| CRITICAL | All 5 JSON parse_only ADMITTED rows are measured against `sonic_rs::from_slice::<Value>` (eager DOM, workload-tagged `eager_typed`) while the row plane is `parse_only`; comparator-misbound. | `skinny/crates/bbnf-bench/benches/json_parity.rs:43-48,87-102` §1.6; `validation/v2-json-validation.md:32-51`; `validation/v6-comparator-integrity.md:14-37` | CONFIRMS V13 |
| CRITICAL | W10.2 vendor_prefixes + custom_at_rules at 34 635 Mbps on 162 B = 37 ns/parse, 124.3× lightningcss — below Criterion overhead floor, criterion (c) fails. | `ROLLING-SOTA-DELTA.md:96-97`; per-parse calc §1.4; v1 §1.5 (parser inspection deferred) | CONFIRMS V13 |
| CRITICAL | W10.3 nested_layout cluster at 52 234 Mbps on 351 B = 54 ns/parse, 123.7×, OVERFIT-THROUGHPUT per v1 §6 + sk-v13 measurement §6; 5 rolling-delta rows share the triple. | `ROLLING-SOTA-DELTA.md:103-106`; per-parse calc §1.4; `sk-v13-audit-overfit-css-measurement.md:258-265` | CONFIRMS V13 |
| HIGH | W2 stylesheet + selectors cluster at 26 895 Mbps on 117 B = 35 ns/parse, 45.1×; 5 rolling-delta rows share the triple, SUSPICIOUS-THROUGHPUT. | `ROLLING-SOTA-DELTA.md:80-82,93-95`; per-parse calc §1.4 | CONFIRMS V13 |
| HIGH | W10.1 at_rules_keyframes + media_queries at 21 585 Mbps on 85 B = 32 ns/parse, 84.9×; 2 rows share the triple. | `ROLLING-SOTA-DELTA.md:83,89`; per-parse calc §1.4 | CONFIRMS V13 |
| MED | 17 of 24 CSS L4 admitted rows fail criterion (b) per-row distinct measurement — they share grouped triples (cluster of 5 / 5 / 5 / 4 / 2 / 2). | `awk` cluster count §1.5 | CONFIRMS V13 |
| MED | `skinny/corpora/css-l4-sk-v13/` (and the SK-V14 successor `skinny/corpora/css-l4-sk-v14/`) absent; no production corpus exists at the path the SK-V13 scoping and the SK-V14 SYNTHESIS §3 C-3 call for. | `ls skinny/corpora/ \| grep -i css` returns `absent` §1.3 | CONFIRMS V13 |

No LOW findings: every measurement-integrity defect already rises to MED or above under the strict reading of the §Scope row A1 criterion.

**New-vs-V13 delta**: zero. Each row above traces back to a verbatim line in v1 / v2 / v6 / sk-v13-audit-overfit-css-measurement.md. The SK-V14 starting state has neither healed nor worsened the V13 disposition; the audit pack's claims remain provable end-to-end against HEAD.

---

## §3 — Pass criterion verdict

> Every ADMITTED row in the prior tranche's ROLLING-SOTA-DELTA has (a) representative corpus ≥ 1 KB, (b) per-row distinct measurement, (c) plausible Mbps relative to byte-throughput ceiling.
> — `PASS-0-OVERFIT-AUDIT.md §Scope` row A1

**FAIL on all three legs.**

- (a) Fails for 24 of 24 CSS L4 admitted rows (largest 357 B vs 1 024 B floor) and for 5 of 5 JSON parse_only admitted rows whose comparator measures a different plane than the row label.
- (b) Fails for 17 of 24 CSS L4 admitted rows (clusters of 5 / 5 / 5 / 4 / 2 / 2 share identical (T1, LC, margin) triples).
- (c) Fails for the 16 admitted rows in clusters W2 / W10.1 / W10.2 / W10.3, whose 31-54 ns/parse cold latencies are below the Criterion measurement-overhead floor; comparator-misbound parse_only rows are unscoreable on (c) by construction.

No additional carries land beyond the V13 audit; the criterion fails wholesale on the same SK-V13-close ledger SK-V14 inherits unchanged.

---

## §4 — Recommended prune actions

Cross-referenced to SK-V14 SYNTHESIS §3 C-1..C-5 (commit `00181742e`):

1. **C-5 (PRUNE-1 + PRUNE-2 — revert) is the binding remediation for every finding above.** PRUNE-1 reverts the 5 JSON parse_only ADMITTED rows (REDRESS cites `v2 §1-4`); PRUNE-2 reverts the 24 CSS L4 ADMITTED rows and deletes the 7 `include_str!`-d hand-written template files (REDRESS cites `v1 §1-6`). The §1.2 census already matches the C-5 acceptance state (CSS L4 0/24, parse_only 0/17 will obtain post-revert without further measurement work).

2. **C-3 (R4 + R5)** must precede any CSS L4 re-admit (R6) — the `cargo xtask regen-css` pipeline consuming the 15 `.bbnf` grammars at `/grammar/css/l4/` is the precondition for grammar-derived parsers, and `skinny/corpora/css-l4-sk-v14/` (~960 KB Bootstrap + Tailwind + Material + Animate) is the precondition for criterion (a) corpus ≥ 1 KB to be satisfiable at all.

3. **C-2 (R1 + R2)** must precede any JSON parse_only re-admit (R7) — the parse_only plane needs a strict-vs-strict comparator (sonic-rs Skipper-class structural-skip per SYNTHESIS §3 R1) replacing the eager-DOM binding at `benches/json_parity.rs:89`; and a per-iteration equality oracle inside the timing region (R2). Until R1 lands, every parse_only Mbps number is provenance-broken on the comparator axis; criterion (c) cannot even be evaluated.

4. **Grouped-measurement disclosure (criterion b)** is not addressed by any current SYNTHESIS candidate. Recommend an addendum to the ROLLING-SOTA-DELTA schema: per-row `measurement_group_id` field, with `xtask gate` rejecting any ADMITTED row whose triple is shared by ≥ 2 rows without an explicit `measurement_group_id` — converts grouped measurement from silent reuse to declared design, satisfying criterion (b) in spirit if not in literal per-row distinctness.

5. **Throughput-plausibility floor.** Recommend the `xtask gate` reject any admitted row whose computed `bytes × 8000 / Mbps` falls below a configurable floor (≥ 100 ns suggested; Criterion warm-up + black_box + sample-timer typically dominates below this). Would auto-flag W2 / W10.1 / W10.2 / W10.3 today; would also catch future regressions of the same pattern.
