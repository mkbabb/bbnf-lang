# SK-V21 Pass 0 — Adversarial Verification of NOW-STATE and FRONTIER Claims (Reports 16–29)

Verifier disposition: skeptical, re-derived from primary sources at HEAD
`734bfec51e30d789851e2d37af9d9b82550a4f72` (branch `master`), from the canonical
csc411 csp-solver tree, and from live web checks dated 2026-07-05. Fifteen
load-bearing claims across the four SK-V21 action axes (stack abrogation,
profiling truth, coordination seams, novelty adjudication) were re-checked. The
net result is that the NOW-STATE claims the plan will act on hold up well — every
file:line the plan would touch was confirmed at HEAD — with two corrections worth
carrying: a genuine cross-report conflict on the serde_json number-decode claim
(Report 21 is right, Report 19 is misleading), and a line-number/scope drift in
Report 20's Lock-14 falsifier claim (substance right, location and "vacuous"
framing overstated).

---

## Confirmed NOW-STATE (act-on-these)

**1. Lock-14 live gate covers only `crates/ir/src` + `crates/analysis/src`
(Report 16 — CONFIRMED).** `crates/ir/tests/lock14_grammar_generalization.rs:83-87`
resolves the scan roots to exactly `vec![ir_crate.join("src"),
crates_dir.join("analysis").join("src")]`; the 17-token ROSTER is at lines 26-44.
`crates/core`, `xtask`, and all of `skinny/` sit outside the gate's reach. The gate
is real (it recursively scans `.rs`, excludes doc-comments, and carries a re-inject
falsifier), but its blast radius is two crates only.

**2. `perf_cost` is a dead literal `0` at exactly one production site (Report 16 —
CONFIRMED).** `skinny/crates/passes/src/lib.rs:590` is the sole production
occurrence (it precedes the `#[cfg(test)]` at line 1674). The other six literal-`0`
occurrences — `lib.rs:2023,2035,2062,2083` and `decision_csp.rs:242,254` — all fall
after `#[cfg(test)]` boundaries (`lib.rs:2012`, `lib.rs:2052`, `decision_csp.rs:226`).
The prior "~8 sites" framing is corrected to "1 production + 6 test", exactly as
Report 16 states.

**3. JSON `CapacityPlan` defaults to `GrowOnly`→256; NEON scan is dead on the live
path (Reports 16 & 21 — CONFIRMED).** `skinny/crates/runtime/src/grammars/json/scan.rs:64`
returns a constant `256` for `GrowOnly`. `neon::scan` is reached only from
`scan_structurals` (line 37), which is invoked only by `structural_capacity_for`'s
`OneShotSimd` arm (line 63). The module header (lines 3-13) documents that the live
default is `GrowOnly` with no env override, so `neon::scan` carries zero live-path
self-time. This is foregone acceleration, not a paid cost — Report 21's framing is
the accurate one.

**4. MASTER-PLAN `MP.SK19.W6` row is stale (Report 16 — CONFIRMED).**
`restart/MASTER-PLAN.md:1041` is the `MP.SK19.W6` row, titled "Lock-2 `StructLayout`
canonical-name reconcile", describing a still-unresolved path-(a) 960-site rename
vs path-(b) side-table, explicitly deferred to "a Pass-Omega/3C call." Meanwhile the
two commits that actually landed under the W6 label — `36ca10646`
("sk-v19-w6abe: collapse the named-grammar leaks — metadata-driven manifest, CSS
host relocation, the live Lock-14 gate") and `f4633d468` ("sk-v19-w6c: re-key the
BBNF shape recognizer on structure, not rule name") — carry an unrelated scope. The
row does not describe what shipped.

**5. bbnf CSP is tautological (Report 18 — CONFIRMED).**
`skinny/crates/passes/src/decision_csp.rs:34-42` computes `selected_index` by finding
the position of the already-chosen `active.facts.selected_candidate_id`; lines 44-47
assign cost `0.0` only to that index and `1.0` to all others; line 52
(`add_selected_constraint(..., selected, selected_index)`) hard-pins the variable to
that index; `backjumping: false` at line 89. The solver recovers the answer it was
handed. The retire-not-port disposition is well-founded.

**6. Lock 4 (egraph→csp direct dependency) is live and contradicts LOCKS prose
(Report 18 — CONFIRMED).** `crates/egraph/Cargo.toml:11` declares
`csp-solver = "0.1"`; `crates/egraph/src/csp_scheduler.rs:30` does `use csp_solver::{...}`
directly. This directly falsifies `restart/locks/LOCKS.md:183` ("The egraph crate has
no direct dependency on csp-solver; ... egraph and csp-solver compose by
output-piping, never by import"). The lock's asserted state is aspirational, not
actual — reinforcing the "locks are prose, not gates" thesis.

**7. Vendored csp-solver is byte-identical to canonical, and the AC-3 arm is present
in both (Report 18 — CONFIRMED).** `diff -rq crates/csp-solver/src` against
`/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src`
returns no differences. The `ac3_from_variable` Unsatisfiable arm
`Revision::Unsatisfiable => return true` is present at `ac3.rs:128` in **both** the
vendored copy and the canonical tree. NOTE: I confirmed the code presence and
byte-identity (the coordination-seam fact SK-V21 acts on — a re-vendor is a
fault-line, not drift-already-happened). I did NOT independently re-derive the "P0
silent false-UNSAT" severity from caller semantics; that characterization is carried
from the csc411 uplift docs, not verified here. The load-bearing NOW-STATE fact (the
line is unfixed in both trees, and the fix lives only in the unmerged tarball) holds.

**8. Two colliding `bbnf-regex@0.1.0` crates (Report 17 — CONFIRMED).**
`/Users/mkbabb/Programming/parse-that/rust/regex/Cargo.toml` is `name = "bbnf-regex"`,
`version = "0.1.0"`, `edition = "2024"`, a full "HIR, NFA→DFA compilation, structural
classification" engine. `skinny/crates/bbnf-regex/Cargo.toml` is also
`name = "bbnf-regex"`, `version = "0.1.0"`, with an **empty `[dependencies]`** section;
`src/lib.rs` is 322 lines exposing `pub fn analyze(pattern: &str) -> RegexFacts`
(line 45). parse-that's Rust tree is frozen at `051a6d6` (2026-05-02); its TypeScript
package is `1.0.0`. The N7 collision is live exactly as described.

**9. skinny is the sole edition-2021 / rust-version-1.78 holdout; nightly features are
switched off (Reports 19 & 29 — CONFIRMED).** `skinny/Cargo.toml:19` is
`edition = "2021"`, `:22` is `rust-version = "1.78"` (the root `crates/` and parse-that
trees are already edition 2024). `.cargo/config.toml` documents that
`-Zthreads=8 -Zshare-generics=y` were removed after a measured 28× warm `iter-check`
regression (0.14s→3.88s), and the cranelift `codegen-backend` override is commented
out pending a pin verification that never landed. The nightly tax is paid for zero
realized benefit — the abrogation-to-stable argument is sound on the primary sources.

**10. Root `[profile.release]` is a Lock-15 violation (Report 19 — CONFIRMED).**
`Cargo.toml:80-85`: `lto = "thin"`, `codegen-units = 1`, `debug = true`, `strip = false`,
`split-debuginfo = "packed"` — with **no `panic = "abort"`** and no `target-cpu`. This is
the exact config Lock 15 documents as halving twitter throughput. The bench profile
below it does use `lto = "fat"`, but the release profile is non-compliant.

---

## Corrections

**11. serde_json number-decode conflict: Report 21 is right, Report 19 is misleading
(CORRECTED).** Report 19 asserts "the skinny runtime delegates JSON number parsing and
string escaping to serde_json ... a latent perf ceiling
(skinny/crates/runtime/src/lib.rs:265; ...)". Re-derivation at HEAD refutes the
hot-path framing: `lib.rs:265` is inside a `#[test]` parity fixture (it compares
skinny's `as_f64()` against `serde_json` for a list of literals), not runtime code.
The live parse hot path decodes numbers **hand-rolled** via
`parse_number_direct → emit_number_direct → materialize_i64/u64/f64`
(`grammars/json/generated.rs:768-838`). serde_json appears in the runtime library only
in the **lazy view / Display** surfaces: `view.rs:232` (`JsonNumber::as_f64`, an
on-demand accessor), `view.rs:130` and `value.rs:94` (`serde_json::to_string` for
key/string serialization) — all off the `parse_only`/`track1` path. Report 21's claim
("the serde_json-carries-the-JSON-figure claim is false; decode is hand-rolled") is
CONFIRMED; Report 19's "delegates ... latent perf ceiling" framing is CORRECTED to:
serde_json is not on the parse hot path; it is confined to the lazy view's `as_f64`
accessor and value/key `Display`.

**12. Report 20's Lock-14 falsifier claim: substance right, line and "vacuous" framing
overstated (CORRECTED).** Report 20 states "Lock 14's falsifier greps 7 crate paths
that do not exist (parse, codegen, runtime, path, path-core, bbnf-simd, parse-that) so
it passes vacuously (restart/locks/LOCKS.md:279)." The Lock-14 verification command is
actually at `LOCKS.md:349`, not `:279` (line 279 is inside a Lock-10/16 regex-facts
block). The command greps
`crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/`.
At HEAD, **8 of those 13 paths are missing** (parse, codegen, runtime, path,
path-core, parse-that-regex, parse-that, bbnf-simd) — one more than the 7 the report
listed (it omitted parse-that-regex). But five DO exist (ir, egraph, csp-solver,
analysis, lsp), so the grep is not *fully* vacuous — it still scans the two crates the
live gate also covers (ir, analysis) plus three more. The corrected statement: the
LOCKS.md verification command references a mostly-nonexistent 24-crate greenfield
topology (8/13 phantom) and is therefore near-vacuous, at line 349, and is a distinct
artifact from the live two-crate test gate of Report 16.

---

## Frontier spot-checks

**13. simdjson current line is 4.6.x; On-Demand default since 1.0 (Report 22 —
CONFIRMED, with a subversion/date correction).** Web (github.com/simdjson releases,
2026-07-05): latest is **v4.6.1** (2026-04-03), v4.6.0 (March 2026). The load-bearing
family claim (4.6.x is the current line; On-Demand is the default frontend) holds. The
report's parenthetical specifics — "4.6.0 2025-03-30 through 4.6.4 2025-05-06" — are
inaccurate: 4.6.0 shipped in **2026** (not 2025), and the latest is 4.6.1, not 4.6.4.
This does not touch the transferable-lever conclusion (materialization/number cost,
not stage-1 SIMD).

**14. serde_json has not adopted Eisel-Lemire; issue #775 open (Report 23 —
CONFIRMED).** GitHub issue serde-rs/json#775 "Update Moderate Path Float Parsing
Algorithms" exists and is the Eisel-Lemire tracking issue, framed as an open
discussion of "multiple approaches" with implementation guidance pointing at
lexical-core's Lemire path. The low-integration-cost lever is real.

**15. Nightly pin resolves to rustc 1.96.0-nightly; stable is 1.96.1; pin ~12 weeks
stale (Report 29 — CONFIRMED).** `rust-toolchain.toml` itself documents the on-host
probe: `nightly-2026-04-11` → `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`. Web
confirms Rust **1.96.0 stable shipped 2026-05-28** and **1.96.1** is the current latest
stable — consistent with the report's "1.96.1 (2026-06-30)". 2026-04-11 → 2026-07-05 is
~12 weeks. All three sub-claims hold; the abrogation-to-pinned-stable framing is
factually grounded.

---

## Verifier's bottom line

Of the fifteen claims re-derived, thirteen are CONFIRMED at HEAD (or at the family
level for the two frontier items) and two are CORRECTED. Neither correction dislodges
an SK-V21 action item: the serde_json correction actually *sharpens* the profiling-truth
axis (the hot-path number decode is our own hand-rolled code, so the SIMD/SWAR
number-parse lever, not a serde_json swap, is the target), and the Lock-14 correction
sharpens the coordination-seam axis (there are two divergent Lock-14 artifacts — a
near-vacuous LOCKS.md prose command over a phantom topology, and a live two-crate test
gate — and neither covers the god-crate `crates/core`). No claim was REFUTED outright;
no NOW-STATE file:line the plan would act on was found to have moved.
