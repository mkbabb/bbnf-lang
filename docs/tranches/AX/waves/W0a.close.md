# AX.W0a.close — Pre-W0b Bench Baseline

**Opens after**: W0a.2.i close (`FINAL.md` written for the W0a window)
**Agents**: 1 serial
**Hard gate**: `docs/benchmarks/post-AX-W0a-close.json` covers the 17-entry bench matrix on master at the immediate pre-W0b state.

## Scope

1. Run the 17-entry bench matrix on master at the W0a-close commit
   (no walker deletion yet, admission widened, shape emission active
   for every grammar). Sequential cold runs per
   `docs/instructions/README.md §Benchmarking`.
2. Aggregate the per-bench throughput into a single JSON artefact at
   `docs/benchmarks/post-AX-W0a-close.json`. Every downstream wave
   (W0b onward) attributes its bench delta against this baseline, not
   against `post-AW-V.json` or `post-AU.json`.
3. Bench discipline per invariant 10 (mid-wave checkpoint): from
   W0b onward, a wave registering ≥ 5% regression vs this baseline
   triggers re-plan.

## File bounds

| File | Access |
|---|---|
| `docs/benchmarks/post-AX-W0a-close.json` | create |
| `docs/benchmarks/post-AX-W0a-close-compile.txt` | create |
| `docs/benchmarks/post-AX-W0a-close-json.txt` | create |
| `docs/benchmarks/post-AX-W0a-close-css.txt` | create |
| `docs/benchmarks/post-AX-W0a-close-sheets.txt` | create |
| `docs/benchmarks/post-AX-W0a-close-bbnf.txt` | create |

Do NOT touch: any `crates/` file (the wave captures a snapshot;
emitter + runtime invariant from W0a.2.i close). Any
`docs/tranches/AX/waves/W0b.md` (W0b opens after this wave).

## Phase sub-items

### AX.W0a.close.1 — 17-entry cold bench run

Mechanism — five bench binaries, sequential, cold per-parse:

```
find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null

cargo bench -p bbnf --bench compile_pipeline > \
  docs/benchmarks/post-AX-W0a-close-compile.txt 2>&1
cargo bench -p bbnf --bench json_monolithic > \
  docs/benchmarks/post-AX-W0a-close-json.txt 2>&1
cargo bench -p bbnf --bench css_l4 > \
  docs/benchmarks/post-AX-W0a-close-css.txt 2>&1
cargo bench -p bbnf --bench google_sheets_monolithic > \
  docs/benchmarks/post-AX-W0a-close-sheets.txt 2>&1
cargo bench -p bbnf --bench bbnf_monolithic > \
  docs/benchmarks/post-AX-W0a-close-bbnf.txt 2>&1
```

Files touched: all five `post-AX-W0a-close-<bench>.txt` outputs.

Sub-gate: each bench binary's stdout contains ≥ N `bench:` lines
where N is the bench's entry count (5 for json, 3 for css, 3 for
sheets, 6 for bbnf, 1 for compile_pipeline). Verified via
`grep -c '^test.*bench:' <file>`.

### AX.W0a.close.2 — aggregation to JSON

Mechanism — parse the five `.txt` outputs and emit a single JSON
document keyed by `(bench, entry) → throughput`:

```json
{
  "commit": "<W0a-close HEAD>",
  "profile": "release",
  "allocator": "mimalloc",
  "date_utc": "<ISO-8601>",
  "entries": {
    "json_monolithic": {
      "data": {"ns_per_iter": <num>, "bytes_per_ns": <num>},
      "twitter": {...},
      "citm": {...},
      "canada": {...},
      "data_xl": {...}
    },
    "css_l4": {
      "normalize": {...},
      "bootstrap": {...},
      "tailwind": {...}
    },
    "google_sheets_monolithic": {
      "parse_simple": {...},
      "parse_nested": {...},
      "parse_stress": {...}
    },
    "bbnf_monolithic": {
      "json": {...},
      "ebnf": {...},
      "css_pretty": {...},
      "google_sheets": {...},
      "bbnf_self": {...},
      "css_l4_grammar": {...}
    },
    "compile_pipeline": {...}
  }
}
```

Files touched: `docs/benchmarks/post-AX-W0a-close.json` — create.

Sub-gate: JSON is well-formed (`python -c 'import json;
json.load(open(...))'` returns 0); `entries` covers the 17-entry
matrix; every entry carries a numeric throughput.

## Hard gate

1. `docs/benchmarks/post-AX-W0a-close.json` exists at the master
   commit that is W0a.2.i's `FINAL.md` HEAD.
2. The JSON `entries` object contains exactly these keys and every
   nested entry carries numeric throughput fields:
   - `json_monolithic` → {`data`, `twitter`, `citm`, `canada`,
     `data_xl`}
   - `css_l4` → {`normalize`, `bootstrap`, `tailwind`}
   - `google_sheets_monolithic` → {`parse_simple`, `parse_nested`,
     `parse_stress`}
   - `bbnf_monolithic` → {`json`, `ebnf`, `css_pretty`,
     `google_sheets`, `bbnf_self`, `css_l4_grammar`}
   - `compile_pipeline` → {`compile`}
3. Five per-bench `.txt` outputs exist under
   `docs/benchmarks/post-AX-W0a-close-{compile,json,css,sheets,bbnf}.txt`.
4. `parse()` routing verified once per grammar via `cargo expand`
   snapshot (tolerated to re-use `post-AX-W0a2{g,h}-expand-*.txt`
   evidence from prior sub-waves if still accurate post-W0a.2.i).

## Verification artefacts

- `docs/benchmarks/post-AX-W0a-close.json` — 17-entry matrix,
  keyed per §Hard gate #2.
- `docs/benchmarks/post-AX-W0a-close-{compile,json,css,sheets,bbnf}.txt`
  — raw `cargo bench` output preserved for reproducibility +
  `criterion`-report re-derivation.
- Orchestrator commit hash recorded in the JSON's `commit` field
  matches `docs/tranches/AX/FINAL.md` W0a-close HEAD citation.

## Dependencies

- **Depends on**: W0a.2.i close (admission widened; `FINAL.md`
  written; `cargo test --workspace` green under shape-emission-
  authoritative routing).
- **Blocks**: W0b. Walker deletion produces a bench delta; without
  this baseline every downstream wave conflates walker-death-cost
  with its own lever attribution.

## Archaeology

`docs/tranches/AX/audit/R4-plan-redress.md §P6` proposed this wave
in response to the W0a.2 cascade. The plan originally sequenced
W0a → W0b without a checkpoint bench; the scope-reveal showed the
17-entry matrix had not been run at any sub-wave close since AW-V
(`post-AW-V.json`). Without a baseline captured between walker
retirement-as-oracle (W0a) and walker deletion-as-code (W0b), every
subsequent wave's ≥ 5% regression threshold (invariant 10) would
have no well-defined reference point.

`post-AW-V.json` is not a substitute. W0a.2's landings (AltDispatch
shape, Array split, inline emission, Keyword Ref-led Alt, `#[inline]`
downgrade on compound shape fns) all change compile-time emission,
which shifts runtime numbers before any walker code deletes. The
baseline must be captured AFTER those landings and BEFORE W0b, not
before both.
