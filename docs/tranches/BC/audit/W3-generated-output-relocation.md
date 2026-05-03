# BC.W3 — Generated Output Relocation Budget

Date: 2026-05-03
Status: settled. Closes surgery 22 (`audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:62`) and G06-6 (`audit/HARDENING-PLAN-2026-05-03-06-generated-code-budget.md:51`).

## §1 Relocation contract

Pre-W3 generated output path: `crates/core/src/grammar/generated/`.
Post-W3e generated output path: `crates/bbnf-parse/src/parse/generated/`.

Per Lane 6, the relocation is path-only. **Bytes unchanged.** **LOC unchanged.** No new emissions land at relocation time. The xtask regen path update (BC.W3e §2.6) is purely a `xtask/src/regen.rs` rewrite.

## §2 Per-grammar relocation table

| Grammar | Pre-W3 path | Post-W3e path | Bytes invariant | LOC invariant |
|---|---|---|---|---|
| `json.rs` | `crates/core/src/grammar/generated/json.rs` | `crates/bbnf-parse/src/parse/generated/json.rs` | byte-identical | LOC unchanged |
| `bbnf.rs` | `crates/core/src/grammar/generated/bbnf.rs` | `crates/bbnf-parse/src/parse/generated/bbnf.rs` | byte-identical | LOC unchanged |
| `css_l4.rs` | `crates/core/src/grammar/generated/css_l4.rs` | `crates/bbnf-parse/src/parse/generated/css_l4.rs` | byte-identical | LOC unchanged |
| `google_sheets.rs` | `crates/core/src/grammar/generated/google_sheets.rs` | `crates/bbnf-parse/src/parse/generated/google_sheets.rs` | byte-identical | LOC unchanged |
| `css_pretty.rs` | `crates/core/src/grammar/generated/css_pretty.rs` | `crates/bbnf-parse/src/parse/generated/css_pretty.rs` | byte-identical | LOC unchanged |
| `ebnf.rs` | `crates/core/src/grammar/generated/ebnf.rs` | `crates/bbnf-parse/src/parse/generated/ebnf.rs` | byte-identical | LOC unchanged |
| `bnf.rs` | `crates/core/src/grammar/generated/bnf.rs` | `crates/bbnf-parse/src/parse/generated/bnf.rs` | byte-identical | LOC unchanged |
| `csv.rs` | `crates/core/src/grammar/generated/csv.rs` | `crates/bbnf-parse/src/parse/generated/csv.rs` | byte-identical | LOC unchanged |
| `math.rs` | `crates/core/src/grammar/generated/math.rs` | `crates/bbnf-parse/src/parse/generated/math.rs` | byte-identical | LOC unchanged |

## §3 Stale-path retirement

After W3e close:

```
rg -n "crates/core/src/grammar/generated" docs/ crates/ xtask/ tests/ -- returns zero (or only archived references with date stamp)
```

Stale references in BC documentation must be updated; the migration cookbook records the canonical post-W3 path.

Per surgery 22, the BC.W6 close gate must NOT reference `crates/core/src/grammar/generated/`; the post-W3 closure path is `crates/bbnf-parse/src/parse/generated/`.

## §4 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W3-G5 (relocation) | Regen-equality at new path | `cargo xtask regen --check` produces byte-identical output to BB close artefact at `crates/bbnf-parse/src/parse/generated/` |
| W3-G6 (LOC delta) | Net delta from BB close ≤ 0% | per-file LOC matches BB close exactly; no growth from relocation alone |
| W3-G7 (stale-path) | No stale path references | `rg -n "crates/core/src/grammar/generated" docs/ crates/ xtask/ tests/ \| grep -v archive` returns zero |

## §5 Cross-references

| Reference | Description |
|---|---|
| `audit/HARDENING-PLAN-2026-05-03-06-generated-code-budget.md:51` (G06-6) | The fault: stale W6 closure row references `crates/core/src/grammar/generated/`; this document corrects via the relocation contract |
| `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:62` (surgery 22) | Surgery 22 mandates the W3 budget gate |
| `feedback_clean_regen_discipline` | Generated files are output of fresh regen; the relocation preserves this discipline |
