# BB.W2a — Cohort Template Specification + Emit

**Thesis** Hereupon the 5-grammar cohort (BNF, CSV, EBNF, CSS Pretty, Math) `runtime/<g>/{document,view,kind,value,mod}.rs` emit from a single codegen template at xtask-regen time per the specification at `docs/tranches/BB/audit/W2-cohort-template-spec.md`. **Closer-gate** BB-G5 met (≥ 1,500 LOC saved); `crates/core/src/codegen/runtime_template.rs` exists; byte-equality precondition gate before deletion of hand-written files (W2c verifies).

## §1 Deliverable

W2a is the cohort template substrate. The five trivial cohort grammars compress from ~2,265 LOC across 35 files to ≤ 250 LOC across 10 files (5 × 2 surviving — arena.rs, builder.rs as template-shimmed). The template at `crates/core/src/codegen/runtime_template.rs` consumes the parameter set documented at `docs/tranches/BB/audit/W2-cohort-template-spec.md` §1 and emits the per-cohort runtime modules.

The byte-equality precondition is critical: M2 verifies template emission produces byte-identical output against the existing hand-written files BEFORE M4 deletes the hand-written files. The discipline is mechanical — diff returns zero before deletion happens.

The cohort-vs-specialised classification reads from `[workspace.metadata.bbnf.grammars.<g>.cohort = true]`; the static `COHORT_GRAMMARS` array (if any) at `crates/core/src/grammar/host.rs:387` deletes; metadata-driven enumeration is the single source-of-truth.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Cohort identification + baseline | Capture pre-W2a LOC; verify BB.W1{a,b,c} closer-gates passed | `find crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math} -name '*.rs' -exec wc -l {} +` matches `audit/CENSUS-2026-05-03.md:507-528` baseline | 35 hand-written cohort files inventoried; deltas computable. |
| M1 | Template scaffolding | Create `crates/core/src/codegen/runtime_template.rs` carrying the per-cohort emission template per the specification at `docs/tranches/BB/audit/W2-cohort-template-spec.md` §5 | `cargo check -p bbnf --profile ax-iter` succeeds; the new file lives in `codegen/` per the layered re-org | Template module exists; xtask wired but not yet emitting. |
| M2 | Byte-equality precondition | Generate the cohort `runtime/<g>/{document,view,kind,value,mod}.rs` files via the template (to a SHADOW directory `runtime/<g>.templated/`); verify byte-equality against existing hand-written files; do NOT yet delete hand-written | `diff -r crates/core/src/runtime/bnf <(xtask regen --grammar bnf --emit-only --shadow)` returns zero diff for all 5 cohort grammars | Template emission matches hand-written byte-for-byte. |
| M3 | Workspace metadata classification | Add `[workspace.metadata.bbnf.grammars.<g>.cohort = true]` for each cohort grammar; the xtask regen pipeline reads metadata to dispatch cohort vs specialised | `cargo metadata --format-version 1 \| jq '.metadata.bbnf'` shows the cohort flag for each | Cohort enumeration is metadata-driven. |
| M4 | Hand-written deletion | Delete the 25 hand-written cohort files (5 × 5 templated files); keep arena.rs + builder.rs as template-shimmed instantiations | `find crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math} -name '*.rs' \| wc -l` returns 10 (5 × 2) | Cohort runtime mass shrinks; templated emission becomes sole source. |
| M5 | xtask regen integration | The xtask regen pipeline emits each cohort grammar's templated files at regen time; whole-workspace `xtask regen --check` ≤ 25 s | `cargo xtask regen --check` runs ≤ 25 s on M1 Pro; the cohort emission accounts for ≤ 1.5 s | xtask iteration-time gate met. |
| M6 | Post-W2a verification | Run `cargo nextest run -p bbnf` 100% pass; verify ≥ 1,500 LOC saved across cohort | All checks pass | BB-G5 met. |
| M7 | Cohort template artefact | Land `docs/tranches/BB/audit/W2a-cohort-template.md` recording per-cohort instantiation parameters + LOC measurements | `test -f docs/tranches/BB/audit/W2a-cohort-template.md` | Template documentation source-of-truth lands. |

## §3 Closer gate

```sh
find crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math} -name '*.rs' | wc -l   # 10
wc -l crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math}/*.rs                  # ≤ 50 LOC each
cargo nextest run -p bbnf --profile ax-iter                                         # 100% pass
cargo xtask regen --check                                                            # ≤ 25 s on M1 Pro
test -f crates/core/src/codegen/runtime_template.rs                                  # template exists
test -f docs/tranches/BB/audit/W2a-cohort-template.md                               # artefact lands
```

## §4 Invariants

§I1. **Lock 13** — cohort grammars compress from ~2,265 LOC to ≤ 250 LOC.

§I2. **Lock 6** — the cohort template emission lands as committed source via xtask regen.

§I3. **Lock 1** — the cohort template carries direct-to-struct emit; no `OpenFrame` instantiation.

§I4. **`feedback_pluggable_components`** — the template is itself a pluggable component; future grammars matching the cohort shape join by metadata declaration.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Cohort template emission drops a behaviour | Medium | M2 byte-equality verification BEFORE M4 deletion; if diff non-zero, template extends to capture variation. |
| Metadata cohort/specialised classification incorrect | Low | Workspace metadata explicitly enumerates each grammar's `cohort` flag; xtask regen asserts at startup. |

## §6 Cross-references

- **BB-G gates closing**: BB-G5 (cohort LOC budget); BB-G11.
- **Carry-tags consumed**: BA→BB.C1, BA→BB.C2, BA→BB.C5.
- **Carry-tags produced**: BB→BC.C2 (cohort template demonstrates direct-to-struct emit grammar-agnostic).
- **Preceding wave**: BB.W1c.
- **Following wave**: BB.W2b (cursor unification across all 9 grammars).

## §7 Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target |
|---|---|---|
| `cargo check -p bbnf --profile ax-iter` | ≤ 11 s | n/a |
| `cargo nextest run -p bbnf --profile ax-iter` | ≤ 90 s | 100% |
| `xtask regen --check` | ≤ 25 s | n/a |
| `xtask regen --grammar <cohort>` | ≤ 1.5 s | n/a |
| `diff -r crates/core/src/runtime/<cohort> <(xtask regen --grammar <cohort> --emit-only)` | ≤ 1 s | byte-equal |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W2a-cohort-template.md` | `docs/tranches/BB/audit/` | Per-cohort instantiation parameters; LOC measurements |
| `W2a-byte-equality-pre-delete.md` | same | M2 byte-equality precondition evidence |
| `W2a-cohort-loc-budget.md` | same | BB-G5 verification; ≥ 1,500 LOC saved |

## §9 Audit lane forecast

| Lane | W2a response |
|---|---|
| Lane 1 | L13 honoured; cohort runtime mass shrinks |
| Lane 2 | M2 byte-equality is same-wave consumer for template substrate |
| Lane 4 | No perf gate (cohort grammars are LOC-only) |
| Lane 5 | Metadata-driven cohort classification; no hardcoded enumeration |
| Lane 6 | Generated parser + runtime template budgets separated per G06-7 |
| Lane 7 | No friction; transparent to grammar authors |
| Lane 8 | BA→BB.C1 closes (BB.W1{a,b,c} + W2a together cover all 9 grammars) |
