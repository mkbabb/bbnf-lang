# SK-V14 W10 JSON parse_only Distinct Path Plan

Status: DISPATCHED

## Entry Gate

- W1 admitted, W9 admitted at `d42b0ae3c`.
- Global PRUNE chain closed before W8-W10 new-admit dispatch.
- Distinct Track 1 path: `runtime::generated_json::parse_only(input: &str)`.
- Generated internal path: `runtime::generated_json::generated::parse_only`.
- Track 2 path: `bbnf_bench::json_parity::track2_structural_oracle` over the independent hand-coded parser.
- Comparator path: `bbnf_bench::sonic_skipper::parse_only` / `sonic_rs::Skipper`.
- Threshold for every row: Track 1 parse_only Mbps must exceed same-run same-corpus `sonic_rs::Skipper` Mbps by at least `+1.0 Mbps`, with per-iteration equality checked inside the timing loop.

## Row Set

All JSON parse_only rows are in scope:

`twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, `y_string_unicode`.

Row ids follow `json/<corpus>/parse_only/main`.

## Stage-0 F-V2-P1ABC-RERECORD

W10 carries Stage-0 unconditionally before any parse_only admit:

- `cargo build --release -p bbnf-bench --features runtime/parse-attribution`
- interactive `samply record` on the W10 parse_only harness path, not `--save-only`
- cfg_attr flip verification at generated JSON attribution sites covering `inline(always)` to `inline(never)` under `parse-attribution`
- consumer manifest: P2-A C6; P2-C C-P2C-3 and C-P2C-8; P2-E Gap 1/3/4/5; P2-F C6/C7/C10/C12/C13

## Implementation Shape

- Add parse_only to the generated JSON public module without allocating `TapeBuilder`, `JsonRoot`, payload storage, or full DOM/tape state.
- Emit the path through the provider-free `runtime_generator.rs` body while keeping the post-W5 retained `json_templates/` surface read-only.
- Keep existing full parse, direct-to-struct, typed product, and comparator rows intact.
- Bench Track 1 parse_only by calling the new generated parse_only path in the existing `track1_generated` criterion lane, preserving row-key compatibility.
- Keep `sonic_rs_skipper` as the strict comparator lane and retain per-iter Track 1 / Track 2 / comparator checks inside the timing loop.

## Redress Conditions

Route to W10 REDRESS instead of ADMIT if:

- the generated path allocates or finishes a full tape,
- Track 1 is a relabel of `runtime::generated_json::parse`,
- comparator evidence is stale, lossy, sidecar-only, or not Skipper-class strict,
- per-iter equality is outside the timing loop,
- Stage-0 evidence is missing or lands after an admit claim,
- full-table maintain fails against the W10 exit gate.
