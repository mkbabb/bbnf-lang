# BB.W0b — Same-Wave Consumer (Minimal Optimiser Smoke Pass)

**Thesis** Hereupon a minimal optimiser smoke pass exercises the BB.W0a path-dep'd sister crates in the SAME wave, structurally precluding the Era V substrate-first / consumer-later anti-pattern per BB02-1 of `audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:39-41`. **Closer-gate** `cargo nextest run -p bbnf-ir --test sister_crate_smoke --profile ax-iter` 100% pass; smoke output written to `docs/tranches/BB/audit/W0b-sister-smoke.json`.

## §1 Deliverable

W0b is the same-wave consumer for W0a's path-dep emigration. The Era V failure mode at `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:8-12` cited substrate-first / consumer-later as the structural anti-pattern; W0a + W0b closes it by landing the consumer in the same wave as the substrate.

The smoke pass exercises three sister crates through the existing `crates/ir/src/passes/` pipeline (no new files):

1. **csp-solver** through type inference: feed a representative grammar fixture (the BBNF self-host grammar from `grammar/bbnf/bbnf.bbnf`) to the existing CSP layout-inference pass; verify the solver returns a layout solution.

2. **egraph** through saturation: take the type-inferred grammar IR, run e-graph saturation over a small rewrite-rule set (the rules already present in `crates/ir/src/rewrites/` excluding rank.rs + tiering.rs which do not exist yet); verify saturation converges.

3. **bbnf-regex** through HIR compilation: take a representative regex literal from a grammar (e.g., the JSON `\d+(\.\d+)?` number regex), compile to HIR via bbnf-regex; verify the HIR shape matches the expected reference.

The smoke output at `docs/tranches/BB/audit/W0b-sister-smoke.json` records the three pass results. BB.W3c verifies the smoke output as a fed-forward fact for cost-model integration; the verification confirms that the path-dep relocation did not introduce regressions in the consumer pipeline.

The smoke pass is NOT a full optimiser pipeline run — that is BB.W3c. W0b's role is to prove that the path-deps RESOLVE and PRODUCE expected output; the W3c full pipeline run validates the cost-model integration.

Estimated edit surface: ~50 LOC test fixture at `crates/ir/tests/sister_crate_smoke.rs`; the test runs the three pass exercises and writes the JSON output. No production code changes.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W0b verification | Verify W0a closer-gate passed; the path-dep'd sister crates resolve | `cargo metadata --format-version 1 \| jq '.workspace_members'` matches W0a expectation | W0a baseline holds. |
| M1 | csp-solver smoke | Create `crates/ir/tests/sister_crate_smoke.rs::csp_smoke`: load the BBNF self-host grammar, run CSP layout inference, assert the solver returns a layout solution | `cargo nextest run -p bbnf-ir --test sister_crate_smoke csp_smoke --profile ax-iter` passes | csp-solver path-dep produces expected output. |
| M2 | egraph smoke | Add `egraph_smoke` test: take W0a's M1 output, run e-graph saturation over the existing rewrite-rule set (excluding rank.rs/tiering.rs), assert convergence | `cargo nextest run -p bbnf-ir --test sister_crate_smoke egraph_smoke --profile ax-iter` passes | egraph path-dep produces expected output. |
| M3 | bbnf-regex smoke | Add `bbnf_regex_smoke` test: take JSON's `\d+(\.\d+)?` regex literal, compile to bbnf-regex HIR, assert the HIR shape matches `tests/fixtures/json_number_hir.txt` | `cargo nextest run -p bbnf-ir --test sister_crate_smoke bbnf_regex_smoke --profile ax-iter` passes | bbnf-regex path-dep produces expected output. |
| M4 | Smoke output artefact | Land `docs/tranches/BB/audit/W0b-sister-smoke.json` with the three pass results; the artefact is read by BB.W3c for cost-model fed-forward verification | `test -f docs/tranches/BB/audit/W0b-sister-smoke.json && jq '.csp,.egraph,.bbnf_regex' docs/tranches/BB/audit/W0b-sister-smoke.json` parses cleanly | Smoke output lands; W3c verification source-of-truth ready. |
| M5 | Era V abrogation evidence | Land `docs/tranches/BB/audit/W0b-era-v-abrogation.md` recording the same-wave producer + consumer relationship: W0a produces path-deps; W0b consumes via smoke pass; the gap between substrate creation and substrate consumption is zero | `test -f docs/tranches/BB/audit/W0b-era-v-abrogation.md` | Era V abrogation gate evidence lands. |

## §3 Closer gate

```sh
cargo nextest run -p bbnf-ir --test sister_crate_smoke --profile ax-iter   # 100% pass
test -f docs/tranches/BB/audit/W0b-sister-smoke.json                       # smoke output lands
test -f docs/tranches/BB/audit/W0b-era-v-abrogation.md                     # abrogation evidence lands
jq '.csp.status, .egraph.status, .bbnf_regex.status' \
   docs/tranches/BB/audit/W0b-sister-smoke.json                            # all "PASS"
cargo nextest run -p bbnf-ir -p bbnf -p bbnf-analysis --profile ax-iter    # workspace nextest 100% pass for BB-owned surfaces (no regression)
```

All five conditions must pass; any failure halts W1a dispatch.

## §4 Invariants

§I1. **Era V abrogation** — W0a's path-dep substrate has a same-wave consumer (W0b's smoke pass); the substrate-first / consumer-later anti-pattern is structurally precluded.

§I2. **Lock 4 precondition** — the three sister crates (csp-solver, egraph, bbnf-regex) prove their path-dep boundary works through the existing pass pipeline; BB.W3c's full optimiser pipeline inherits a verified substrate.

§I3. **Lock 11** — each sister crate's path-dep status is exercised, not merely declared; the workspace metadata + smoke pass form a complete L11 verification.

## §5 Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| The smoke pass discovers a path-dep resolution issue that W0a's `cargo check` did not surface (e.g., a transitive-dep version conflict) | Low | `cargo nextest run` errors with "no matching version" | The W0b test is the exact mechanism that surfaces such issues; remediation is to update the workspace `[patch.crates-io]` table per W0a M5. |
| The smoke pass test fixtures drift from the actual grammar shape (e.g., the BBNF grammar at `grammar/bbnf/bbnf.bbnf` evolves and the smoke pass references a stale rule name) | Low | `cargo nextest run` errors with "rule not found" | The fixture references are anchored to the W0a-generated-baseline; the smoke pass updates with grammar evolution as a routine maintenance concern. |
| The smoke output JSON format differs from BB.W3c's expectation | Low | BB.W3c entry preflight fails to parse the smoke output | The format is documented at `docs/tranches/BB/audit/W0b-sister-smoke.json` schema-section; BB.W3c verifies `jq '.csp.layout_solution_count, .egraph.saturation_iterations, .bbnf_regex.hir_node_count'` parse + structural conformance. |

## §6 Cross-references

- **BB-G gates this wave is on the path to closing**: BB-G10 (optimiser composition output-piped — proof-of-concept here).
- **Carry-tags this wave consumes**: (W0a outputs only)
- **Carry-tags this wave produces**: BB→BC.C1 precursor — the optimiser pipeline shape becomes evident.
- **Preceding wave dependency**: BB.W0a — sister crates emigrated; metadata recorded.
- **Following wave consumer**: BB.W3c — the full optimiser pipeline run; reads `W0b-sister-smoke.json` to verify path-deps still produce identical output.

## §7 Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target |
|---|---|---|
| `cargo nextest run -p bbnf-ir --test sister_crate_smoke --profile ax-iter` | ≤ 8 s | 100% |
| `cargo nextest run -p bbnf-ir -p bbnf -p bbnf-analysis --profile ax-iter` | ≤ 90 s | 100% |
| `jq '.csp.status' docs/tranches/BB/audit/W0b-sister-smoke.json` | ≤ 1 s | n/a |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W0b-sister-smoke.json` | `docs/tranches/BB/audit/` | Three pass results (CSP layout solution; egraph saturation iterations; bbnf-regex HIR node count); BB.W3c reads for fed-forward verification |
| `W0b-era-v-abrogation.md` | same | The same-wave producer/consumer evidence; the gap between W0a substrate creation and W0b smoke consumption is zero |

## §9 Audit lane forecast

| Lane | Anticipated challenge | W0b response |
|---|---|---|
| Lane 1 | Era V failure mode resurgence | W0b IS the abrogation: substrate (W0a) + consumer (W0b) land in the same wave |
| Lane 2 | "Does W0b have its own consumer?" | W0b's consumer is BB.W3c (next wave); the verdict per `docs/PHASE-4-DIRECTIVE-2026-05-03.md:330-334` is OK because next-wave consumer is acceptable when same-wave is structurally impossible (the smoke pass cannot consume itself) |
| Lane 4 | W0b has no perf gate | The smoke pass is a structural correctness gate, not a perf gate; perf gates live at W3c + W6 |
| Lane 5 | Per-grammar leaks? | The smoke pass references one grammar (BBNF) by name in the test fixture; this is a TEST grammar reference, not a production reference; the per-grammar enumeration is metadata-driven everywhere else |
| Lane 8 | "Does W0b close any carry?" | W0b consumes W0a's substrate same-wave; no carry between W0a and W0b |

## §10 Implementation reference

The smoke pass test:

```rust
// crates/ir/tests/sister_crate_smoke.rs (created at W0b.M1-M3)
use bbnf_ir::passes::layout::run_layout_inference;
use egraph::EGraph;
use bbnf_regex::compile_to_hir;

#[test]
fn csp_smoke() {
    let grammar = load_test_fixture("bbnf");
    let layout = run_layout_inference(&grammar).expect("CSP layout inference must converge");
    assert!(!layout.is_empty(), "layout solution must be non-empty");
    write_smoke_output("csp", &layout);
}

#[test]
fn egraph_smoke() {
    let grammar = load_test_fixture("bbnf");
    let mut egraph = EGraph::new();
    egraph.add_grammar(&grammar);
    let saturation_result = egraph.saturate_with_existing_rewrites(); // excludes rank.rs/tiering.rs (W3c)
    assert!(saturation_result.iterations > 0, "egraph saturation must do work");
    write_smoke_output("egraph", &saturation_result);
}

#[test]
fn bbnf_regex_smoke() {
    let regex_literal = r"\d+(\.\d+)?";
    let hir = compile_to_hir(regex_literal).expect("bbnf-regex HIR compilation must succeed");
    let expected_node_count = 5; // {Concat, Repeat<Class<digit>>, Optional<Concat<Lit<.>, Repeat<Class<digit>>>>}
    assert_eq!(hir.node_count(), expected_node_count, "HIR shape must match");
    write_smoke_output("bbnf_regex", &hir);
}

fn write_smoke_output(pass: &str, output: &impl Serialize) {
    let path = format!("docs/tranches/BB/audit/W0b-sister-smoke.json");
    let mut json: serde_json::Value = std::fs::read_to_string(&path)
        .map(|s| serde_json::from_str(&s).unwrap_or_default())
        .unwrap_or_default();
    json[pass] = serde_json::to_value(output).unwrap();
    std::fs::write(&path, serde_json::to_string_pretty(&json).unwrap()).unwrap();
}
```

The W0b commit body MUST include the JSON output from the test run (the smoke pass results); the file at `docs/tranches/BB/audit/W0b-sister-smoke.json` is the single source-of-truth that BB.W3c reads for fed-forward verification.
