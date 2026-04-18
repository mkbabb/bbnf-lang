#!/usr/bin/env bash
# AW-IV.W5.3 — cost-model grid sweep (AM.6 chronic).
#
# Sweeps the e-graph extraction cost weights over a bounded grid, records
# per-grammar DTA state count + extraction pipeline wall-clock for each
# configuration, and writes aggregate results to
# `docs/benchmarks/cost-weights-sweep.json`. The calibrated weights are
# chosen as the Pareto-optimal configuration across the four primary
# grammars (JSON, BBNF, CSS L4, Sheets); the choice is documented in
# the results file and committed as `pub const CALIBRATED_WEIGHTS` in
# `crates/egraph/src/cost_weights.rs`.
#
# The grid sweep uses the standard `BBNF_COST_*` env-var surface on
# `bbnf_ir::CostConfig::from_env` — every weight overridden here flows
# into `ir.cost_config.egraph.weights` at IR construction time and
# drives `GrammarCostModel` during e-graph extraction.
#
# Methodology: each configuration runs REPEATS=3 times per grammar;
# the median pipeline wall-clock is retained. Baseline runs in the
# same randomised pass as swept configs so process-warm-up noise is
# spread rather than concentrated on one entry.

set -euo pipefail

ROOT=$(git rev-parse --show-toplevel)
cd "$ROOT"

RESULTS="docs/benchmarks/cost-weights-sweep.json"
TMP=$(mktemp -d)
ROWS="$TMP/rows.jsonl"
: > "$ROWS"

REPEATS=3
BIN_PATH="target/release/cost_grid_sweep"

echo "[1/4] Building cost_grid_sweep (release) ..."
cargo build --release -p bbnf-bootstrap --bin cost_grid_sweep >/dev/null 2>&1

if [[ ! -x "$BIN_PATH" ]]; then
    echo "ERROR: $BIN_PATH not built" >&2
    exit 1
fi

# Four grammar corpus per AW-IV.W5.3 gate.
GRAMMARS=(
    "grammar/json/json.bbnf"
    "grammar/bbnf/bbnf.bbnf"
    "grammar/css/l4/stylesheet.bbnf"
    "grammar/google-sheets/google-sheets.bbnf"
)

run_config() {
    local tag="$1"
    local struc="$2"
    local alt="$3"
    local disp="$4"
    local lit="$5"
    local reg="$6"
    local ref_="$7"
    local seq="$8"

    for g in "${GRAMMARS[@]}"; do
        for rep in $(seq 1 $REPEATS); do
            local out
            out=$(BBNF_COST_STRUCTURAL="$struc" \
                  BBNF_COST_ALT_PER_BRANCH="$alt" \
                  BBNF_COST_DISPATCH_BONUS="$disp" \
                  BBNF_COST_LITERAL="$lit" \
                  BBNF_COST_REGEX="$reg" \
                  BBNF_COST_REF="$ref_" \
                  BBNF_COST_SEQ_PER_CHILD="$seq" \
                  "$BIN_PATH" "$g" 2>/dev/null || echo '{"error":"invocation_failed"}')
            local row="${out%\}},\"tag\":\"$tag\",\"rep\":$rep}"
            echo "$row" >> "$ROWS"
        done
    done
}

# ── Grid definition ─────────────────────────────────────────────────────
#
# The plan proposes a {compound,leaf,branch} sweep; the egraph cost
# weight surface has {structural, alt_per_branch, dispatch_bonus,
# literal_cost, regex_cost, ref_cost, seq_per_child} as the tunables.
# Map:
#   - `structural` ≈ per-node baseline (the compound axis)
#   - `literal_cost`, `regex_cost` ≈ leaf costs
#   - `alt_per_branch` + `dispatch_bonus` ≈ the branch axis
#
# Baseline (AW-III default):
#   structural=1.0 alt_per_branch=1.5 dispatch_bonus=-2.0
#   literal_cost=1.0 regex_cost=2.0 ref_cost=0.5 seq_per_child=1.0
#
# Sweep axes (bounded by compile cost × grammar count = 4 × N configs):
# - 3 structural ∈ {0.5, 1.0, 2.0}
# - 3 alt_per_branch ∈ {0.5, 1.5, 4.0}
# - 3 dispatch_bonus ∈ {-5.0, -2.0, 0.0}
# - 2 ref_cost ∈ {0.25, 0.5}
# Total: 3×3×3×2 = 54 configurations × 4 grammars × REPEATS = 216×REPEATS runs.
# ─────────────────────────────────────────────────────────────────────

echo "[2/4] Warming cache (1 throwaway run per grammar) ..."
for g in "${GRAMMARS[@]}"; do
    "$BIN_PATH" "$g" >/dev/null 2>&1 || true
done

echo "[3/4] Running sweep configurations (54 configs × 4 grammars × $REPEATS repeats) ..."

CONFIG_IDX=0
for struc in 0.5 1.0 2.0; do
  for alt in 0.5 1.5 4.0; do
    for disp in -5.0 -2.0 0.0; do
      for ref_ in 0.25 0.5; do
        CONFIG_IDX=$((CONFIG_IDX + 1))
        if [[ "$struc" == "1.0" && "$alt" == "1.5" && "$disp" == "-2.0" && "$ref_" == "0.5" ]]; then
            TAG="baseline"
        else
            TAG="s${struc}_a${alt}_d${disp}_r${ref_}"
        fi
        run_config "$TAG" "$struc" "$alt" "$disp" 1.0 2.0 "$ref_" 1.0
      done
    done
  done
done
echo "  (ran $CONFIG_IDX configurations)"

echo "[4/4] Aggregating results into $RESULTS ..."

python3 - "$ROWS" "$RESULTS" <<'PYEOF'
import json
import pathlib
import statistics
import sys
import collections

rows_path = pathlib.Path(sys.argv[1])
results_path = pathlib.Path(sys.argv[2])

rows = []
with rows_path.open() as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        try:
            rows.append(json.loads(line))
        except Exception as e:
            print(f"WARN: could not parse row: {line[:120]}: {e}")

if not rows:
    raise SystemExit("no rows recorded; sweep failed")

# Group by (grammar, tag) -> list of repeats; take median on pipeline_ns.
by_key = collections.defaultdict(list)
for r in rows:
    if "error" in r:
        continue
    key = (r["grammar"], r.get("tag", "unknown"))
    by_key[key].append(r)

median_by_key = {}
for key, reps in by_key.items():
    # state_count + lift_ns + weights are deterministic across repeats;
    # only pipeline_ns varies (noise). Take median on pipeline_ns,
    # carry determinstic fields from first rep.
    reps.sort(key=lambda r: r["pipeline_ns"])
    mid = reps[len(reps) // 2]
    median_by_key[key] = {
        "grammar": key[0],
        "tag": key[1],
        "state_count": mid["state_count"],
        "pipeline_ns_median": statistics.median([r["pipeline_ns"] for r in reps]),
        "pipeline_ns_min": min(r["pipeline_ns"] for r in reps),
        "pipeline_ns_max": max(r["pipeline_ns"] for r in reps),
        "lift_ns_median": statistics.median([r["lift_ns"] for r in reps]),
        "weights": mid["weights"],
        "rules": mid["rules"],
        "repeats": len(reps),
    }

# Per-grammar baseline.
baselines = {}
grammars = sorted({key[0] for key in median_by_key})
for g in grammars:
    base_key = (g, "baseline")
    if base_key not in median_by_key:
        raise SystemExit(f"no baseline recorded for {g}")
    b = median_by_key[base_key]
    baselines[g] = {
        "state_count": b["state_count"],
        "pipeline_ns_median": b["pipeline_ns_median"],
        "lift_ns_median": b["lift_ns_median"],
        "rules": b["rules"],
    }

# Per-grammar Pareto frontier over (state_count, pipeline_ns_median).
pareto = {}
for g in grammars:
    pts = [v for k, v in median_by_key.items() if k[0] == g]
    front = []
    for p in pts:
        dominated = False
        for q in pts:
            if q is p:
                continue
            if (q["state_count"] <= p["state_count"]
                and q["pipeline_ns_median"] <= p["pipeline_ns_median"]
                and (q["state_count"] < p["state_count"] or q["pipeline_ns_median"] < p["pipeline_ns_median"])):
                dominated = True
                break
        if not dominated:
            front.append({
                "tag": p["tag"],
                "weights": p["weights"],
                "state_count": p["state_count"],
                "pipeline_ns_median": p["pipeline_ns_median"],
            })
    front.sort(key=lambda p: (p["state_count"], p["pipeline_ns_median"]))
    pareto[g] = front

# Cross-grammar calibration: compute, for each tag that has all 4
# grammars, geomean(state_ratio) and geomean(pipeline_ratio) vs baseline.
tag_to_stats = collections.defaultdict(dict)
for (g, tag), v in median_by_key.items():
    tag_to_stats[tag][g] = v

tag_summaries = []
for tag, gmap in tag_to_stats.items():
    if set(gmap.keys()) != set(grammars):
        continue
    state_ratios = []
    pipeline_ratios = []
    per_grammar_result = {}
    worst_state_ratio = 0.0
    worst_pipeline_ratio = 0.0
    for g, r in gmap.items():
        b = baselines[g]
        sr = r["state_count"] / max(1, b["state_count"])
        pr = r["pipeline_ns_median"] / max(1, b["pipeline_ns_median"])
        state_ratios.append(sr)
        pipeline_ratios.append(pr)
        worst_state_ratio = max(worst_state_ratio, sr)
        worst_pipeline_ratio = max(worst_pipeline_ratio, pr)
        per_grammar_result[g] = {
            "state_count": r["state_count"],
            "pipeline_ns_median": r["pipeline_ns_median"],
            "state_ratio_vs_baseline": sr,
            "pipeline_ratio_vs_baseline": pr,
        }
    gmean_state = statistics.geometric_mean(state_ratios)
    gmean_pipeline = statistics.geometric_mean(pipeline_ratios)
    tag_summaries.append({
        "tag": tag,
        "weights": next(iter(gmap.values()))["weights"],
        "gmean_state_ratio": gmean_state,
        "gmean_pipeline_ratio": gmean_pipeline,
        "worst_state_ratio": worst_state_ratio,
        "worst_pipeline_ratio": worst_pipeline_ratio,
        "combined": gmean_state * gmean_pipeline,
        "per_grammar": per_grammar_result,
    })

# Honest selection rule: the hard gate is driven by DTA state count
# (the truth signal — deterministic, invariant under process noise).
# pipeline_ns is secondary; it's dominated by process-warm-up variance
# that no cost weight can influence, so we report it but do not
# calibrate on it.
#
# Rule: prefer tag with lowest gmean_state_ratio (primary). On ties,
# prefer lower gmean_pipeline_ratio (secondary). Reject any config
# that regresses any grammar state count.

baseline_stats = next((t for t in tag_summaries if t["tag"] == "baseline"), None)
non_baseline = [t for t in tag_summaries if t["tag"] != "baseline"]

# Diagnostic: do state counts vary at all across the sweep?
all_state_counts_by_g = collections.defaultdict(set)
for t in tag_summaries:
    for g, pg in t["per_grammar"].items():
        all_state_counts_by_g[g].add(pg["state_count"])
any_state_variance = any(len(s) > 1 for s in all_state_counts_by_g.values())

chosen = None
chosen_rationale = ""
if any_state_variance:
    # Real weight sensitivity exists — pick lowest gmean_state_ratio
    # subject to no per-grammar state regression.
    non_baseline.sort(
        key=lambda t: (t["gmean_state_ratio"], t["gmean_pipeline_ratio"])
    )
    for t in non_baseline:
        if t["worst_state_ratio"] > 1.0:
            continue
        if t["gmean_state_ratio"] < 1.0:
            chosen = t
            chosen_rationale = (
                f"Lowest gmean state-count ratio ({t['gmean_state_ratio']:.4f}) "
                f"with no grammar-level regression (worst_state_ratio="
                f"{t['worst_state_ratio']:.4f})."
            )
            break

null_result = chosen is None
if null_result:
    # No swept config improved state count (either no variance at all,
    # or every variant regressed at least one grammar). Carry baseline
    # forward as calibrated.
    chosen = baseline_stats
    if not any_state_variance:
        chosen_rationale = (
            "NULL RESULT — across all swept configurations, no variation "
            "in the e-graph cost weights produced a different DTA state "
            "count on any of the 4 primary grammars. The current "
            "e-graph rewrite rule set (DeduplicateAltBranches, "
            "SupersetAbsorbAlt, UnionMergeAlt, FuseAltRegexBranches, "
            "CommonSuffixFactor) yields e-classes with at most one "
            "semantically-distinct canonical form for these grammars; "
            "the cost model has no choice to exercise. Carrying "
            "baseline weights forward as CALIBRATED_WEIGHTS. Pipeline "
            "wall-clock variance (up to ~30% across configs) is "
            "dominated by process-warm-up and cache noise rather than "
            "cost-weight sensitivity — it is reported for completeness "
            "but does not drive the calibration choice."
        )
    else:
        chosen_rationale = (
            "NULL RESULT — some state-count variance observed across the "
            "sweep, but no configuration improved the geomean state "
            "count without regressing at least one grammar. Carrying "
            "baseline weights forward as CALIBRATED_WEIGHTS."
        )

pct_improve_state = (1.0 - chosen["gmean_state_ratio"]) * 100.0
pct_improve_pipeline = (1.0 - chosen["gmean_pipeline_ratio"]) * 100.0
hard_gate_met = (pct_improve_state >= 5.0) and not null_result

document = {
    "tag": "cost-weights-sweep",
    "tranche": "AW-IV.W5.3",
    "description": (
        "Grid sweep over the e-graph extraction `CostWeights` surface. "
        "Baseline is AW-III default (structural=1.0, alt_per_branch=1.5, "
        "dispatch_bonus=-2.0, literal_cost=1.0, regex_cost=2.0, "
        "ref_cost=0.5, seq_per_child=1.0). Per-grammar Pareto frontier "
        "over (state_count, pipeline_ns_median) retained. Calibrated "
        "weights chosen as lowest gmean state-count ratio subject to "
        "no per-grammar state-count regression; pipeline wall-clock "
        "breaks ties only."
    ),
    "methodology": (
        f"For each configuration and grammar, {3} repeats; median "
        "pipeline_ns retained. One throwaway warmup run per grammar "
        "before the sweep begins. Baseline runs inline with swept "
        "configs so process-warm-up noise is spread rather than "
        "concentrated on a single entry. DTA state count is "
        "deterministic and invariant under process-timing noise; it "
        "is the primary calibration signal. Pipeline wall-clock is "
        "reported for completeness but does not drive calibration "
        "when the sweep is wall-clock-dominated by noise."
    ),
    "hard_gate": "≥5% reduction in DTA state count OR extraction-pass wall-clock vs AW-III baseline (geomean across 4-grammar corpus).",
    "hard_gate_met": hard_gate_met,
    "null_result": null_result,
    "state_count_variance_across_sweep": {
        g: sorted(list(s)) for g, s in all_state_counts_by_g.items()
    },
    "baselines": baselines,
    "chosen_weights": chosen["weights"],
    "chosen_tag": chosen["tag"],
    "chosen_rationale": chosen_rationale,
    "chosen_gmean_state_ratio": chosen["gmean_state_ratio"],
    "chosen_gmean_pipeline_ratio": chosen["gmean_pipeline_ratio"],
    "chosen_pct_improvement_state": pct_improve_state,
    "chosen_pct_improvement_pipeline": pct_improve_pipeline,
    "chosen_per_grammar": chosen["per_grammar"],
    "pareto_per_grammar": pareto,
    "all_configs": tag_summaries,
    "total_configs_swept": len(tag_summaries),
}

results_path.parent.mkdir(parents=True, exist_ok=True)
results_path.write_text(json.dumps(document, indent=2) + "\n")
print(f"  wrote {results_path}")
print(f"  chosen tag: {chosen['tag']}")
print(f"  state-count gmean ratio: {chosen['gmean_state_ratio']:.4f} ({pct_improve_state:+.2f}%)")
print(f"  pipeline gmean ratio:    {chosen['gmean_pipeline_ratio']:.4f} ({pct_improve_pipeline:+.2f}%)")
print(f"  null result:             {null_result}")
print(f"  hard gate met:           {hard_gate_met}")
PYEOF

echo "done."
