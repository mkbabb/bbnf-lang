# 05 — Per-wave bench-checkpoint protocol

The contract a W{0–6} bench-closure agent consumes verbatim.
AV's single operational miss was intra-tranche bench absence;
the rules below make absence structurally impossible.

## 1. Bench-agent input surface

The orchestrator passes one dispatch message per wave:

| Field | Source |
|-------|--------|
| `wave_id` | literal `"W0" … "W6"` (W7 no bench) |
| `wave_commit` | `git rev-parse HEAD` at wave close |
| `prior_artefact` | `docs/benchmarks/post-AV.json` (W0) else `post-AW-W{N-1}.json` |
| `bench_matrix` | four binaries / 19 entries (§3.1) |
| `wave_gate` | verbatim AW.md §Hard gates summary W{N} |
| `levers_in_wave` | ordered list of AW.md §Phase items closed |
| `grammar_profile_calibration` | `setup_floor_ns` + `expected_ns_per_byte` per grammar (W4+) |
| `cwd` | worktree `crates/core` (bench binaries are cwd-sensitive — PROFILING.md) |
| `shared_target_dir` | absolute `$CARGO_TARGET_DIR` |
| `output_json` | `docs/benchmarks/post-AW-W{N}.json` |

Agent is **read-only on source** — sole writes are the JSON
artefact + a one-paragraph PROGRESS.md commentary at `## Wave
W{N} bench checkpoint` heading.

## 2. Command sequence (verbatim)

Per README §Benchmarking + §Cache clearing + §Expensive
commands — always file-first:

```bash
cd "$CWD"                                   # crates/core
find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null

# Prebuild once if wave.tsv absent (prepare-profile-wave.sh does this too)
bash ../../scripts/prebuild-benches.sh

# Four parse-bench invocations — sequential, never parallel
cargo bench -p bbnf --bench json_monolithic          > /tmp/bench-json.txt   2>&1
cargo bench -p bbnf --bench css_l4                   > /tmp/bench-css.txt    2>&1
cargo bench -p bbnf --bench google_sheets_monolithic > /tmp/bench-sheets.txt 2>&1
cargo bench -p bbnf --bench bbnf_monolithic          > /tmp/bench-bbnf.txt   2>&1

grep -n 'bench:' /tmp/bench-*.txt > /tmp/bench-summary.txt
```

Rules: cold per-parse only (mimalloc verified in every bench —
`json/monolithic.rs:4`, `css/l4.rs:4`,
`google_sheets/monolithic.rs:8`, `bbnf/monolithic.rs:9`); never
racing commands; file-first, grep `/tmp/bench-*.txt` instead of
re-running; shared `CARGO_TARGET_DIR` per PROFILING.md.

## 3. `post-AW-W{N}.json` schema

Adapts post-AV/AU shape; adds `prior`, `deltas`, `attribution`,
`gate_status`. Top-level keys:

```json
{
  "tag":           "post-AW-W2",
  "tranche":       "AW",
  "wave":          "W2",
  "date":          "2026-04-XX",
  "commit":        "<40-hex>",
  "prior_commit":  "<40-hex>",
  "prior_tag":     "post-AW-W1",
  "arch":          "aarch64-apple-darwin",
  "wave_gate":     "AW.md §Hard gates summary W2 verbatim",
  "levers_closed": ["psi_rayon", "shape_ref", "bug_2b_residuals"],
  "benches":       { /* §3.1 */ },
  "gate_status":   { /* §3.2 */ },
  "regression_rationales": { },
  "samply_attribution_sidecar": null,
  "bootstrap_idempotent": true,
  "honesty_note": "one-paragraph prose, post-AV.json register"
}
```

### 3.1 Entry matrix — 19 entries

Confirmed against Cargo.toml `[[bench]]` + each
`benchmark_group!` call:

| Binary | Entries |
|--------|---------|
| `json_monolithic` | `data_s, twitter, citm, canada, data_xl` |
| `css_l4` | `normalize, bootstrap, tailwind` |
| `google_sheets_monolithic` | `parse.{parse_simple,parse_nested,parse_stress}, format.{format_simple,format_stress}` |
| `bbnf_monolithic` | `json, ebnf, css_pretty, google_sheets, bbnf_self, css_l4_grammar` |

Worked example — `json_monolithic.canada` post-W1:

```json
"json_monolithic": {
  "canada": {
    "ns_per_iter": 2431008,
    "mb_per_s":    927,
    "prior":       { "ns_per_iter": 4947301, "mb_per_s": 455 },
    "delta_mb_s":  472,
    "delta_pct":   103.7,
    "gate":        { "target_mb_s": 1231,
                     "status":      "post-AU baseline met" },
    "attribution": {
      "primary":   "dta_activate",
      "secondary": "stage_c_cond",
      "residual":  "AV.3.5 Eisel-Lemire already wired; string decode awaits W3"
    }
  }
}
```

Sheets `parse.*` / `format.*` keep post-AV's nested shape.

### 3.2 `gate_status`

```json
"gate_status": {
  "W2.bootstrap_700_mbps":            { "met": true,  "actual": 742 },
  "W2.twitter_decode_self_time_5pct": { "met": false, "actual": "7.2%",
                                        "sidecar": "samply_attribution_sidecar" }
}
```

Gates whose text cites `self_time` always carry a
`sidecar` reference (§4 heavy attribution).

## 4. Attribution contract

Compact lever enum (strings are greppable across artefacts):

```
stage_c_cond         — W0.1  Stage-C conditional gate
span_elision         — W0.3  Span-rule emitter elision
aggregate_right_size — W0.4  __aggregate_buf right-sizing
color_view           — W0.5  LargeAggregate Color admission + view
fuse_acyclic         — W0.10 inline_acyclic / fuse_single_use guard drop
dta_activate         — W1    DTA driver + fn-per-rule deletion
psi_rayon            — W2.1  per-grammar parallel_break_even_bytes
shape_ref            — W2.3  SHAPE_DICT runtime dispatch
phf_keyword          — W3.1  PHF for namedColor + Sheets fn names
simd_compare         — W3.2  SIMD keyword compare on ≤16-entry Alts
selector_classifier  — W3.3  CSS selector classifier
scanner_padded       — W3.4  find_next_structural + scan_quoted PaddedView
parallel_fork        — W4.1–3 list-rule fork + chunk detect + offset remap
bloom_dedup          — W4.4–5 runtime bloom + GADT dedup
pratt_lower          — W4.6  Sheets Pratt precedence-tower collapse
profile_calibration  — W4.7  GrammarProfile slot calibration
visitor_reduce       — W6.1  Tape::reduce_column API + specialisation
visitor_simd_pack    — W6.3  portable_simd f64x4 packing
```

**Light derivation (default).** From `levers_in_wave` metadata
alone: single-lever waves map mechanically; multi-lever waves
rank by expected-magnitude per AW.md phase narrative (e.g. W1
ranks `dta_activate > stage_c_cond` — AW.md §Phase 1 calls
the DTA swap "the lever that recovers the regression in one
stroke"). No samply required.

**Heavy derivation (optional sidecar).** When the gate text
contains `self_time` (W2 twitter `decode_json_string <5%`, W3
`__compoundSelector <15%`), the agent MAY run
`scripts/profile-bench-headless.sh` on the single (bench,entry)
pair with the prebuilt binary per PROFILING.md —
`--unstable-presymbolicate`, never `--save-only`; artefact
under `.profiles/samply/W{N}/`. Evidence path lands in
`samply_attribution_sidecar.<entry>`. Recommend samply mandate
on W2 + W3 self-time gates only; discretionary elsewhere.

## 5. Regression rationale

When any entry's `delta_pct < 0` vs prior, the artefact's
`regression_rationales` map grows:

```json
"regression_rationales": {
  "css_l4.bootstrap": {
    "delta_pct": -8.3,
    "expected":  true,
    "reason":    "W2.1 PSI fork-gate adds branch per list-rule below parallel_break_even_bytes (bootstrap 114KB < 120KB calibrated). W4.1 document-level fork overwrites this gate above threshold.",
    "routing":   "self-healing-in-W4",
    "samply_evidence": null
  }
}
```

Minimum content: `delta_pct`, `expected: bool`, `reason` (cite
AW.md phase narrative or Substrate-cost ledger row), `routing`
∈ { `self-healing-in-W{N+k}`, `accept-as-correctness` (Bug-1 /
i64-f64 / empty-compound rows AW.md §Substrate-cost ledger
marks permanent), `blocker` }, `samply_evidence`.

**Reopen policy (mechanical).** Unrationalised regression
reopens the wave; orchestrator gate fails. `routing: blocker`
reopens. Self-healing or correctness passes; orchestrator
carries the entry forward to FINAL.md. Rationale **content**
(prose, AW.md citation) stays orchestrator-reviewed (§9).

## 6. PROGRESS.md commentary format

One paragraph at `## Wave W{N} bench checkpoint —
YYYY-MM-DD` heading. Minimum content:

- Headline ΔMB/s vs prior (e.g. "canada 455→927 MB/s +104%;
  bootstrap 182→742 MB/s +307%").
- Dominant-mover lever via attribution enum + residual.
- Per-gate pass/fail one-liner.
- Each regression: one-sentence rationale + routing tag.
- `post-AW-W{N}.json` commit hash for FINAL.md lift.

No numbers without artefact backing; declarative register per
post-AV.json §honesty_note.

## 7. Small-input amortisation (AW.4.7)

Bench reports MB/s. For entries where
`prior.ns_per_iter < dta_setup_floor_ns × 10` (setup ≥ 10% of
total time), the W{N≥4} artefact computes:

```
expected_mb_s = (input_bytes × 1e9)
              / (dta_setup_floor_ns + input_bytes × expected_ns_per_byte)
achieved_expected_ratio = actual_mb_s / expected_mb_s
```

Per-entry JSON gains:

```json
"small_input_amortisation": {
  "applied":                 true,
  "dta_setup_floor_ns":      380,
  "expected_ns_per_byte":    1.8,
  "expected_mb_s":           472,
  "achieved_expected_ratio": 0.94
}
```

Constants source:
- **Post-W4** (W5+, W6): parse `const GRAMMAR_PROFILE` literal
  in each grammar's `generated.rs` (AW.4.7 commits values).
- **Pre-W4** (W0–W3): `applied: false`, fixed AW.md MB/s gates
  apply. Setup-pathology only manifests after W1 deletes the
  fn-per-rule ambient overhead that hides it today.

Setup-dominated candidates: Sheets `parse_simple` (5µs /
0.5KB), BBNF `json` (6µs / 0.5KB), `format_simple` (~150ns),
`normalize` (~8µs / 6KB). Large-input entries (canada,
tailwind, bootstrap, data_xl) retain fixed gates.

## 8. Multi-checkpoint composition — post-AW.json

**Recommendation: enriched trajectory history**, not a bare
post-AW-W6 copy.

```json
{
  "tag":        "post-AW",
  "tranche":    "AW",
  "commit":     "<W6 close>",
  "benches":    { /* W6 entries verbatim — no re-bench */ },
  "gate_status":{ /* AW.md §Hard gates summary W6 full table */ },
  "multi_wave_history": {
    "post-AV":    { "canada": 455,  "bootstrap": 182 },
    "post-AW-W0": { "canada": 582,  "bootstrap": 247 },
    "post-AW-W1": { "canada": 1245, "bootstrap": 521 },
    "post-AW-W2": { "canada": 1782, "bootstrap": 742 },
    "post-AW-W3": { "canada": 1821, "bootstrap": 918 },
    "post-AW-W4": { "canada": 2054, "bootstrap": 943 },
    "post-AW-W5": { "canada": 2047, "bootstrap": 941 },
    "post-AW-W6": { "canada": 2063, "bootstrap": 944 }
  },
  "trajectory_note": "per-entry recovery visible in-artefact; FINAL.md cites this map for the recovery narrative"
}
```

Rationale: FINAL.md cites wave-by-wave trajectory; W6-bare
forces FINAL.md to re-open every W{N}.json. Embedded map
makes composition mechanical; no re-bench — `jq`-aggregated
from post-AV + the eight W{0–6} files.

## 9. Open questions for orchestrator

1. **Heavy-attribution cadence.** Mandate samply sidecar only
   on self-time gates (W2 + W3), discretionary elsewhere — or
   mandate on every wave? Mandate adds ~15min/wave; discretion
   matches AV posture. Recommend self-time-mandate.
2. **Rationale prose review.** Mechanical reopen on missing
   rationale is clean; content quality stays orchestrator-
   reviewed. Should a separate review agent validate AW.md
   citations before gate-pass?
3. **Multi-checkpoint aggregator.** §8 enrichment — inline
   `jq` by orchestrator or dedicated aggregator agent? Agent
   catches schema drift across eight files; `jq` is faster.
4. **Sub-100-µs noise floor.** Bencher harness dominates at
   150ns (`format_simple`). Raw `ns_per_iter` in artefact,
   amortisation-ratio in PROGRESS paragraph — or the inverse?

## Citations

- `docs/tranches/AW/AW.md` §§Wave schedule, Bench-checkpoint
  contract, AW.4.7, Hard gates summary, Architectural invariants
  3+8, Substrate-cost ledger.
- `docs/instructions/README.md` §§Benchmarking, Cache clearing,
  Expensive commands — always file-first.
- `docs/instructions/PROFILING.md` §§Shared-target, Samply
  invocation rules, Orchestration contract.
- `docs/benchmarks/post-AV.json`, `post-AU.json` (schema + baseline).
- `scripts/{prebuild-benches,prepare-profile-wave,profile-bench-headless}.sh`.
- `crates/core/Cargo.toml` `[[bench]]` table; each bench's
  `benchmark_group!` call (19 entries); `#[global_allocator] mimalloc`.
- Memory: `no-warm-benches`, `bench-single-run`,
  `bench-sequential-regression`, `test-output-to-file`.
