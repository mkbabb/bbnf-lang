# AZ-IV W0 BA/BB Coverage Ledger

**Status**: refreshed 2026-05-01 by AZ-IV.W0.1 Truth Ledger. Every BA/BB
wave hard gate is mapped to **absorbed** (lands inside AZ-IV's
grammar-derived StructDirect route), **retired** (incompatible with the
AZ-IV thesis with artefact-backed decision), or **routed** (explicitly
outside AZ-IV with a named successor). Evidence pointers cite the
landing AZ-IV plan section, wave spec, or recycled-BA spec.

## Definitions

- **Absorbed** — the requirement lands inside AZ-IV; evidence cites the
  AZ-IV.md carry-ledger row, non-routable carry row, hard gate, or wave
  spec that owns it.
- **Retired** — the requirement is incompatible with the AZ-IV thesis
  (no second parser, path, optimizer, or runtime path) and has an
  artefact-backed decision; evidence cites the GESTALT.md or AZ-IV.md
  passage that records the retirement.
- **Routed** — the requirement is post-AZ-IV scope with a named
  successor; evidence cites the recycled BA spec
  (`docs/tranches/BA/BA.md`) or the cross-repo future-work section in
  `docs/GESTALT.md` §5. Routing is invalid when the item can fit AZ-IV
  without creating a parallel parser/path/optimizer system.

Sources read for this refresh: `docs/tranches/AZ-IV/AZ-IV.md` §Carry
Ledger / §Non-Routable Carries / §Hard Gates; `docs/tranches/AZ-IV/
waves/W0..W6.md`; `docs/tranches/BA/BA.md` (recycled letter);
`docs/tranches/BA/historical/BA-pre-recycle.md` and
`docs/tranches/BA/historical/waves-pre-recycle/W0..W3.md`;
`docs/tranches/BB/BB.md` (subsumed banner) and
`docs/tranches/BB/waves/W0..W4.md`; `docs/tranches/BB/waves/W5.md` and
`W6.md` (BC.W5/BC.W6 scope).

## Scope split

The pre-recycle BA tranche covered typed pointer-path queries over
struct trees with isomorphic Rust + TS + Python bindings. AZ-IV pass-4
absorbs that scope and recycles the BA letter for the post-AZ-IV
rule-discovery successor (formerly the BB tranche). BB itself is
subsumed: rule-discovery work flows into the recycled BA letter; perf
and value-API items flow into AZ-IV.

## Pre-recycle BA Coverage (typed pointer-path scope)

Source documents: `docs/tranches/BA/historical/BA-pre-recycle.md` and
`docs/tranches/BA/historical/waves-pre-recycle/W0..W3.md`.

| Source | Requirement | AZ-IV disposition | Owner | Evidence |
|---|---|---|---|---|
| BA.W-1 | proof of AZ direct-struct output, no tape path, no stale fallback resolver | absorbed | W0 | `AZ-IV.md` §Carry Ledger row 1 (regen drift); `GESTALT.md` §1 (tape runtime gone) |
| BA.W0 | `Path`, `PathSegment`, `TypedPath<G, T>` typed path construction | absorbed | W2 | `AZ-IV.md` §Carry Ledger row "Path IR + path! macro"; §Non-Routable Carry 27; `waves/W2.md` File Bounds row `crates/core/src/path/{ir,type_check,...}` |
| BA.W0 | path type checker against `StructRegistry` (segment + struct + alternatives diagnostic) | absorbed | W2 | `AZ-IV.md` §Hard Gates 7 (compile-time `path!` macro); `waves/W2.md` `path_check` IR pass |
| BA.W0 | parent-pointer micro-bench across in-struct / root-traversal / hybrid sidecar | absorbed | W2 | `AZ-IV.md` §Non-Routable Carry 29 (AscentStrategy hybrid sidecar); §Hard Gates 22 (default sidecar chosen by W2 micro-bench) |
| BA.W0 | IR `path_check` pass over `path!` sites | absorbed | W2 | `AZ-IV.md` §Non-Routable Carry 28 (`path_check` IR pass after `project_types` + inline-trace sidecar); `waves/W2.md` File Bounds `crates/ir/src/passes/path_check.rs` |
| BA.W1 | lazy traversal executor over the struct tree (descent + ascent) | absorbed | W3 | `AZ-IV.md` §Carry Ledger row "Lazy bail-out parse"; §Hard Gates 9 (parse_with on JSON, CSS L4, Sheets, BBNF); `waves/W3.md` `crates/core/src/path/executor.rs` |
| BA.W1 | `NodeView<'p, T>` borrowed view semantics | absorbed | W2 + W3 | `GESTALT.md` §4 item 4 (wildcard returns lazy `Iter<Item = T>`); `AZ-IV.md` §Hard Gates 11 |
| BA.W1 | `path!` proc-macro emits compile-time typed accessor | absorbed | W2 | `AZ-IV.md` §Hard Gates 7 (compile-time `path!`); §Non-Routable Carry 27; `waves/W2.md` `crates/bbnf-path/src/path_macro.rs` |
| BA.W1 | wildcard / no-collect traversal | absorbed | W2 | `AZ-IV.md` §Invariants 10 (wildcard returns lazy iterators); §Hard Gates 11; §Non-Routable Carry 33; `GESTALT.md` §4 item 4 |
| BA.W1 | 3-field JSON path extraction at least 20 % faster than sonic-rs `pointer!` | absorbed | W6 | `AZ-IV.md` §Carry Ledger row "Sonic-rs same-harness performance floor"; §Hard Gates 16 (lazy lane closes <= 5x sonic on `bbnf_get_*`); `waves/W6.md` (post-AZ-IV.json) |
| BA.W1 | 30-field JSON access parity-or-better vs sonic-rs `pointer!` | absorbed | W6 | same as above; same-harness comparator row in `post-AZ-IV.json` |
| BA.W1 | sonic-rs / simdjson / cssparser / lightningcss parity for path output | absorbed | W1 + W6 | `AZ-IV.md` §Invariants 5 (semantic parity is current); §Hard Gates 4 (parity gates); `waves/W1.md` parity matrix; `waves/W6.md` competitor harness |
| BA.W2 | TS template-literal tag isomorphic to Rust `path!` macro | absorbed | W5 | `AZ-IV.md` §Carry Ledger row "TS template-literal tag binding"; §Non-Routable Carry 31; §Hard Gates 20 (TS binding executable); `waves/W5.md` `crates/bbnf-path-ts/` cdylib |
| BA.W2 | Python callable path surface (`crates/bbnf-path-py/`) | retired | W5 | `AZ-IV.md` §Deletion Bias item "no Python binding path (Python is dropped from the thesis per Q-final-4)"; rationale: backend matrix is Rust + TS + WASM only, so a Python binding cannot land without a parallel host-binding system that contradicts the AZ-IV thesis |
| BA.W2 | host-binding isomorphism across errors and values | absorbed | W5 | `AZ-IV.md` §Hard Gates 20 (isomorphic error taxonomy with Rust frontend); `waves/W5.md` agent unit covering TS host-binding ledger |
| BA.W3 | full measurement matrix (4 grammars x 3 / 10 / 30 fields, cold) | absorbed | W6 | `AZ-IV.md` §Hard Gates 15 (post-AZ-IV.json schema); `waves/W6.md` post-AZ-IV.json + samply 7-artefact contract |
| BA.W3 | extended parity harness across sonic-rs / simdjson / cssparser / lightningcss | absorbed | W1 + W6 | `AZ-IV.md` §Hard Gates 4 (parity gates current and green); `waves/W6.md` competitor harness rows |
| BA.W3 | FINAL.md handoff to BB | retired | W6 | superseded by AZ-IV.md §Cross-Tranche Debt: BA recycled for rule discovery, BB subsumed. AZ-IV.W6 FINAL.md replaces the handoff |
| BA critical files | `crates/core/src/path/*` + `crates/bbnf-path*` file layout | absorbed | W2 + W5 | `AZ-IV.md` §Critical Files And Ownership rows "Path IR + macro + AscentStrategy" (W2) and "TS binding + value-API dedup + substrate audit" (W5) |

## BB Coverage (rule-discovery + perf scope)

Source documents: `docs/tranches/BB/BB.md` (subsumed banner) and
`docs/tranches/BB/waves/W0..W4.md`. BB.W5 / W6 in the directory are
labelled BC.W5 / BC.W6 and cover replay/resume/debug tooling, distinct
from BB's e-graph rule inference scope; they route as their own line
items.

| Source | Requirement | AZ-IV disposition | Owner | Evidence |
|---|---|---|---|---|
| BB preflight | AZ-III carries: regen drift, `Map` preservation, tailwind, watchdogs | absorbed | W0 + W4 + W6 | `AZ-IV.md` §Carry Ledger rows 1, 2, 5, 6 |
| BB thesis | egraph-first rewrite inference, VM residue oracle second | routed | recycled BA | `AZ-IV.md` §Cross-Tranche Debt (BA recycled for rule discovery); `BA.md` §Architectural thesis items 1-2; `BA.md` §AZ-IV dependency (hard opening gate) |
| BB.W0 | `crates/ir/src/rewrites/` schema, registry, ranker, tiering | routed | recycled BA W0 | `BA.md` §Storage architecture; §Critical files; `AZ-IV.md` §Hard Gates 14 (`RuleSet` deleted; BA recreates clean) |
| BB.W0 | Ruler CVC enumerator over `IrNode` (`crates/egraph/src/ruler/enumerate.rs`) | routed | recycled BA W0 | `BA.md` §Critical files; `AZ-IV.md` §Carry Ledger row "Rewrite/ruler substrate unconsumed" (deletion sets up clean recreation in BA) |
| BB.W0 | egraph residue split (`ruler/residue.rs`) | routed | recycled BA W0 | same |
| BB.W0 | VM oracle wrapper (`ruler/oracle.rs`) | routed | recycled BA W0 | `BA.md` §VM-as-oracle |
| BB.W0 | base fleet-wide rules (RON files in `crates/ir/src/rewrites/base/`) | routed | recycled BA W0 | `BA.md` §Storage architecture; §Critical files |
| BB.W0 | grammar-rewrite discovery in `cargo xtask regen` | routed | recycled BA W0 | `BA.md` §Storage architecture (`grammar/<name>/rewrites/*.ron`); §Critical files |
| BB.W0 | Tranche H soundness rediscovery >= 80 % | routed | recycled BA W0 | `BA.md` §Hard gates "Soundness rediscovery" |
| BB.W0 | retained-rule corpus hit-rate >= 0.1 firings/parse | routed | recycled BA W0 | `BA.md` §Hard gates "Corpus hit-rate measurement" |
| BB.W1 | JSON enumeration run with >= 5 accepted sound rules | routed | recycled BA W1 | `BA.md` §Wave structure W1 (JSON + Sheets curated batch) |
| BB.W1 | Sheets enumeration run with >= 5 accepted sound rules | routed | recycled BA W1 | same |
| BB.W1 | generated.rs re-emission and shrink verification | routed | recycled BA W1 + W4 | `BA.md` §Hard gates "Cost gates" (>= 10 LOC shrink on at least one grammar) |
| BB.W1 | review ledger (per-rule docs + audit log) | routed | recycled BA W1+ | `BA.md` §Ranker + tiered review §Review surface |
| BB.W2 | CSS L4 wide-alphabet target N=50, >= 5 accepted rules | routed | recycled BA W2 | `BA.md` §Wave structure W2 |
| BB.W2 | BBNF wide-alphabet target N=40, >= 5 accepted rules | routed | recycled BA W2 | same |
| BB.W2 | enumerator node-count ceiling and rejection-rate control | routed | recycled BA W2 | `BA.md` §Reversal criteria item 5 (e-graph node-count ceiling) |
| BB.W3 | every production grammar has rewrite-dir decision | routed | recycled BA W3 | `BA.md` §Wave structure W3 |
| BB.W3 | grammar-colocated rule authoring under `grammar/<name>/rewrites/` | routed | recycled BA W3 | `BA.md` §Storage architecture §Extensibility statement |
| BB.W3 | synthetic-grammar extensibility with zero core/emitter edits | routed | recycled BA W3 | same |
| BB.W4 | cost-model integration (rank/tier feeds extraction/cost) | routed | recycled BA W4 | `BA.md` §Wave structure W4 (cost-model integration) |
| BB.W4 | CI auto-accept job for Class-1 rules | routed | recycled BA W4 | `BA.md` §Wave structure W4 |
| BB.W4 | ranker calibration to >= 90 % Class 1+2 | routed | recycled BA W4 | `BA.md` §Ranker + tiered review |
| BB.W4 | review ledger close + FINAL.md | routed | recycled BA W4 | same |
| BB hard gates | rejection <= 50 %; accepted Class 1+2 >= 90 %; generated shrink; throughput gain; parity after each accepted rule | routed | recycled BA every wave | `BA.md` §Hard gates (rule-inference / ranker / cost / parity / storage gates) |
| BB perf — per-grammar value-enum dedup (structural skeleton) | absorbed | W5 | `AZ-IV.md` §Carry Ledger row "Per-grammar value-enum dedup"; §Non-Routable Carry 25; §Hard Gates 21; `GESTALT.md` §4 item 7 (typed enums preserved) |
| BB perf — sonic-rs same-harness floor on JSON value/path | absorbed | W6 | `AZ-IV.md` §Carry Ledger row "Sonic-rs same-harness performance floor"; §Non-Routable Carry 7; §Hard Gates 16 |
| BB perf — watchdog row resolution under fat-LTO | absorbed | W6 | `AZ-IV.md` §Non-Routable Carry 6 (zero watchdog rows on fat-LTO + bench-iter); §Hard Gates 15 |
| BB perf — tailwind regex_scan timeout | absorbed | W4 | `AZ-IV.md` §Carry Ledger row "Tailwind regex_scan perf timeout"; §Non-Routable Carry 5; `waves/W4.md` |

## BC.W5 / BC.W6 Coverage (debug + minimise + replay)

`docs/tranches/BB/waves/W5.md` and `W6.md` carry headers labelled
`BC.W5` and `BC.W6`. They cover debug + inspect + minimise tooling
(W5) and FINAL bounded-cost closure (W6) over a hypothetical BC
substrate. They are not part of BB's rule-inference scope and route
to a different successor.

| Source | Requirement | AZ-IV disposition | Owner | Evidence |
|---|---|---|---|---|
| BC.W5 | debug + inspect + minimise tooling over production provenance/replay surfaces | routed | post-AZ-IV future tranche (debug + minimise scope) | not in AZ-IV scope; AZ-IV invariant 12 retires legacy tooling, not opens new tooling tranches. `GESTALT.md` §5 future-work cross-repo motion records the post-AZ-IV residue, but debug + minimise tooling specifically routes to a future tranche dedicated to that scope (no successor letter assigned at AZ-IV close) |
| BC.W6 | FINAL + bounded-cost closure for replay/resume/incremental/recovery/debug tooling | routed | same future tranche as BC.W5 | same; not absorbed into AZ-IV because the surfaces named (replay, resume, incremental recovery) are not AZ-IV substrate |

## AZ-III named carries — direct cross-check

Cross-checking every NAMED-CARRY row in `docs/tranches/REMAINING-
TRAJECTORY.md` "Live Blockers" table against the AZ-IV carry ledger.

| AZ-III carry | AZ-IV destination | Evidence |
|---|---|---|
| regen-pipeline strict-mode drift (4 substrate divergences) | W0 | `AZ-IV.md` §Carry Ledger row 1 (Strict regen drift); §Hard Gates 1 |
| egraph cost extractor strips `Map { fn_id }` | W0 | §Carry Ledger row 2 (Egraph `Map { fn_id }` preservation); §Hard Gates 5 |
| 11 Sheets parity tests + 1 ts_backend_emits_discriminated_union | W1 + W5 | §Carry Ledger row "Sheets parity gap"; row "TS discriminated union test" |
| tailwind regex perf timeout | W4 | §Carry Ledger row "Tailwind regex_scan perf timeout"; §Non-Routable Carry 5 |
| 17-entry matrix + cross-profile fat-LTO refresh | W6 | §Carry Ledger row "Cross-profile watchdog bench rows"; §Non-Routable Carry 6; §Hard Gates 15 |
| WASM/derive residue (parse-that + wasm + xtask) | W0 | §Carry Ledger row "WASM/derive residue"; W0.md scope items 9-10; W0.md hard gate 10 (`bbnf_derive` zero-hit gate) |

Every AZ-III carry has an AZ-IV destination with an artefact-bound
hard gate.

## Routing legitimacy check

Routing is invalid when an item can fit AZ-IV without creating a
parallel parser/path/optimizer system. Cross-checking each routed BB
row above against the AZ-IV thesis:

- BB rule-discovery scope (Ruler CVC enumerator, VM oracle on residue,
  ranker, Class-1/2/3 tiering, `crates/ir/src/rewrites/`) cannot fit
  AZ-IV without expanding scope to discover and evaluate fleet-wide
  rewrite rules. The mechanism (`crates/egraph/src/ruler/`) is a new
  substrate that AZ-IV explicitly deletes (§Hard Gates 14: BA
  recreates clean). Routing to recycled BA is legitimate.
- Pre-recycle BA Python binding is retired (not routed) because the
  AZ-IV thesis caps the host-binding matrix at Rust + TS + WASM per
  Q-final-4 (`AZ-IV.md` §Deletion Bias).
- BC.W5 / BC.W6 debug + minimise tooling routes to a future tranche
  because the named surfaces (`runtime::debug`, `runtime::minimise`,
  `crates/core/tests/debug_trace_contract.rs`) are not AZ-IV substrate.
  AZ-IV ships compile-time `path!` typing, lazy bail-out parse, value-
  API dedup, and TS binding; debug tooling over those is post-AZ-IV.
  No successor letter is assigned at AZ-IV close per `AZ-IV.md`
  §Cross-Repo Future Work (which records out-of-AZ-IV scope motion).

## Closure

Every pre-recycle BA wave hard gate, every BB wave hard gate, and the
auxiliary BC.W5 / BC.W6 scope have a disposition row. Zero rows close
as `unknown`, `later`, or ownerless. Zero rows route to a successor
letter without naming the successor and the blocker proof. The
post-AZ-IV residue is one tranche of pure rule-discovery work
(recycled BA) plus a future tranche for debug + minimise tooling
(unassigned letter).
