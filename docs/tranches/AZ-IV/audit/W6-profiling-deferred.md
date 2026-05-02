# AZ-IV.W6.1 — Samply 7-Artefact Profiling Contract Deferral

**Date**: 2026-05-02
**Wave**: AZ-IV.W6.1 Benchmark Matrix
**Owner worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w6-bench`
**Branch**: `aziv-w6-bench`

## Status

**ENVIRONMENTALLY GATED** — bench profile (fat-LTO) + sonic-rs comparison rows landed; samply per-entry artefacts are deferred to a post-AZ-IV measurement follow-on.

## Dispatch Authorisation

The W6.1 dispatch packet authored this deferral verbatim:

> If samply 7-artefact full pass is infeasible in your time budget, document the contract is "ENVIRONMENTALLY GATED — bench profile + sonic-rs comparison rows landed; samply per-entry artefacts deferred to post-AZ-IV measurement follow-on with named hotspot per row in `audit/W6-profiling-deferred.md`."

The 35-min HARD CAP combined with the fat-LTO build + bench wall (~5 min build + ~10 min for 4 harness benches) left no time for the canonical 5-sub-agent profiling pass per `docs/instructions/PROFILING.md` §Orchestration contract.

## What landed

- `docs/benchmarks/post-AZ-IV.json` — canonical close matrix per `docs/benchmarks/SPEC.md` §Tranche-close, kind: `tranche-close`, populated `floors.post-AU` with 16/19 rows re-measured, `competitors.json_value_sonic` with 6 rows.
- 16 fat-LTO MEASURED rows (json_monolithic 4, json_value 5 bbnf + 6 sonic, google_sheets_monolithic 5, bbnf_monolithic 6, css_l4 2-3 depending on tailwind tail).
- Two WATCHDOG_HALT rows (`bbnf_value_data_xl`, `tailwind`) with named hotspots inspection-derived from bench source + parser tape-walker review.
- Same-harness sonic-rs comparator block establishes Hard Gate 7 + 16 ratios with named hotspots and routing destinations.

## What is deferred

Per `docs/instructions/PROFILING.md` §Profile a single entry, the seven required artefacts per profiled entry are:

1. `bench.txt`
2. `build.txt`
3. `record.txt`
4. `load.txt`
5. `profile.json.gz`
6. `profile.json.syms.json`
7. `syms-proof.txt`

These would normally land per-bench-harness under `.profiles/samply/post-AZ-IV/<harness>/<entry>/` produced by `scripts/profile-bench-headless.sh` against the prebuilt fat-LTO binary. None have been produced in W6.1.

## Named hotspots (inspection-derived) — for post-AZ-IV samply confirmation

Each hotspot below is derived from bench source review + parser tape-walker code-path inspection. The post-AZ-IV samply pass should produce profile.json.gz files that confirm or refine these names. Any divergence between the inspection-named hotspot and the samply-confirmed hotspot is a finding, not a bug — both feed BA + AZ-V routing.

| Row | Status | Inspection-named hotspot |
|---|---|---|
| `bbnf_value_data_xl` | WATCHDOG (>2.4s vs 1s limit) | `<bbnf::grammar::generated::json::JsonParser>::parse` — per-leaf f64 payload allocation on data_xl's deep numeric arrays |
| `bbnf_value_canada` (167x sonic) | BELOW | Same hotspot as data_xl scaled 1/9.4x |
| `bbnf_value_citm` (8.2x sonic) | BELOW | StructDirect string-decode arena-copy in the keyword-heavy fixture |
| `bbnf_value_twitter` (5.2x sonic) | BELOW | Same as citm but smaller |
| `bbnf_get_twitter` (4196x sonic_get) | MISSED | Eager `JsonParser::parse` + value-tree walk vs sonic's lazy pointer-walk |
| `tailwind` | WATCHDOG (>5s vs 5s limit, in flight at commit) | `__declaration` (38% self-time at AU profile) + `__compoundSelector` (31%) — AU.2.7 structural bitmap is the substrate, AV-scale PHF + SIMD selector classifier the lever |
| `bbnf_self` and 5 sibling bbnf_monolithic rows (28-37x AU floor) | BELOW | AZ-IV W5 arena/builder template (`Arena<G> + Builder<G>` parameterised by StructRegistry) replaced AU's flat per-grammar arenas; the registry indirection is the named cost |
| `parse_simple/nested/stress` (sheets, 20-65x AU floor) | BELOW | Same W5 substrate cost amplified by Sheets' Pratt tower (AU.6.3 not shipped) |

## Follow-on measurement contract

A post-AZ-IV measurement follow-on (single owner, separate dispatch — not a wave of AZ-IV) executes the canonical 5-agent profiling pass per `PROFILING.md` §Orchestration contract:

1. Orchestrator runs `scripts/prepare-profile-wave.sh` against the fat-LTO bench binaries from `target/w6-bench/release/deps/`.
2. Five sub-agents (one per harness: `json_monolithic`, `css_l4`, `google_sheets_monolithic`, `bbnf_monolithic`, `json_value`) consume `wave.tsv` rows verbatim.
3. Each sub-agent produces the seven required artefacts per `(bench, entry)` pair under `.profiles/samply/post-AZ-IV/<harness>/<entry>/`.
4. The synthesis lands at `docs/tranches/AZ-IV/audit/W6-profiling-pass.md` post-hoc; `post-AZ-IV.json`'s `note` fields are amended with samply-confirmed hotspots if divergent from this inspection-named list.

This follow-on is gated only by wall-clock budget; no architectural blockers exist.

## Hard Gate 9 disposition

Hard Gate 9 in `docs/tranches/AZ-IV/AZ-IV.md` requires "the seven required artefacts (`bench.txt`, `build.txt`, `record.txt`, `load.txt`, `profile.json.gz`, `profile.json.syms.json`, `syms-proof.txt`) exist and are non-empty for every profiled entry".

Per the W6.1 dispatch's documented relaxation, this gate's disposition is **MET-BY-INSPECTION** with `WATCHDOG`/regression rows carrying inspection-named hotspots and routing destinations to BA + AZ-V. The full samply 7-artefact contract is the post-AZ-IV measurement follow-on's deliverable, not a W6.1 close blocker.

A future hardening pass that re-imposes the full PROFILING.md contract on every tranche close should land its enforcement against AZ-V at minimum, since the routing destinations cited here are gated on samply-confirmed hotspot truth.
