# BB.W3a — CSP Layout Passes Path-Dep Wiring

**Thesis** Hereupon the existing CSP layout-inference passes (renamed from `passes/types/` to `passes/layout/` per surgery 5 + BA→BB.C2 carry) wire through the `csp-solver` path-dep crate; output piped to W3b's e-graph + miner stage. **Closer-gate** `cargo nextest run -p bbnf-ir --test layout_pipe` 100% pass; `rg -n 'passes/types/' crates/ir/src/` returns zero; `rg -n 'TypeDesc\|StructLayout\|LayoutDesc' crates/ir/src/` returns zero.

## §1 Deliverable

W3a is the first of three W3 sub-waves. The optimiser pipeline composes by output-piping per Lock 4; W3a establishes the first stage (CSP layout inference) as the substrate for the subsequent stages.

Per surgery 5 of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:49`, the pass directory rename: `crates/ir/src/passes/types/` → `crates/ir/src/passes/layout/`. The rename is part of the BA→BB.C2 carry (Layout/LayoutSink canon); BA.W2 retired the aliases (`TypeDesc`, `StructLayout`, `LayoutDesc`); W3a verifies the canon holds at the IR layer.

The csp-solver path-dep wiring: the layout-inference pass calls into the path-dep'd `csp-solver` crate; the output `LayoutSolution` becomes the input to W3b's e-graph saturation.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W3a verification | Verify W2c closer-gate passed; csp-solver path-dep resolves (W0a/W0b verified) | `cargo metadata` shows csp-solver as path-dep | Substrate ready. |
| M1 | Pass directory rename | Rename `crates/ir/src/passes/types/` → `crates/ir/src/passes/layout/`; update all imports | `rg -n 'passes/types/' crates/ir/src/` returns 0 | Layout canon holds at the IR layer. |
| M2 | Alias verification | Verify `TypeDesc`, `StructLayout`, `LayoutDesc` aliases are extinct (BA.W2 retiral verified) | `rg -n 'TypeDesc\|StructLayout\|LayoutDesc' crates/ir/src/` returns 0 | L2 (Layout canon) honoured. |
| M3 | csp-solver wiring | The layout-inference pass calls into csp-solver path-dep; the output `LayoutSolution` is consumable by W3b | `cargo nextest run -p bbnf-ir --test layout_pipe` passes | csp-solver path-dep is exercised through the pass pipeline. |
| M4 | Output-pipe artefact | Land `docs/tranches/BB/audit/W3a-layout-pipe.md` recording the layout-inference output schema | `test -f docs/tranches/BB/audit/W3a-layout-pipe.md` | W3b reads the schema; output-piping documented. |

## §3 Closer gate

```sh
rg -n 'passes/types/' crates/ir/src/                                              # 0
rg -n 'TypeDesc|StructLayout|LayoutDesc' crates/ir/src/                           # 0
cargo nextest run -p bbnf-ir --test layout_pipe --profile ax-iter                 # 100% pass
test -f docs/tranches/BB/audit/W3a-layout-pipe.md                                  # artefact lands
```

## §4 Invariants

§I1. **Lock 2** — Layout canon holds; old terms extinct.
§I2. **Lock 4** — output-piping precondition; layout pass is the first piped stage.
§I3. **Lock 11** — csp-solver path-dep is exercised, not declared.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Old alias references survive in tests or bench code | Low | M2 grep covers all of `crates/ir/src/`; tests + benches check via separate grep at W3a M2. |

## §6 Cross-references

- **Carry-tags consumed**: BA→BB.C2.
- **Following wave**: BB.W3b.

## §7 Iter-time check

| Cargo Command | Expected Duration |
|---|---|
| `cargo nextest run -p bbnf-ir --profile ax-iter` | ≤ 50 s |
| `cargo check -p bbnf-ir --profile ax-iter` | ≤ 9 s |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W3a-layout-pipe.md` | `docs/tranches/BB/audit/` | Layout-inference output schema; W3b read source |

## §9 Audit lane forecast

| Lane | Response |
|---|---|
| Lane 1 | L2, L4 honoured |
| Lane 4 | No perf gate (structural change) |
