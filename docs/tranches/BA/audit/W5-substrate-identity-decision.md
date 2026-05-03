# BA.W5 Substrate Identity Decision — Lock 1 Disposition

Date: 2026-05-03
Decision: **(b) BA migrates JSON only; Lock 1 deferred-with-receiver at BA close.**

## §1 — Choice

BA migrates **JSON only** to direct-to-struct. CSS L4, BBNF, Sheets, and the five-grammar template cohort (BNF, CSV, EBNF, CSS Pretty, Math) retain `OpenFrame` substrate at BA close. Lock 1 is **deferred-with-receiver**: BB.W1a (CSS L4), BB.W1b (BBNF), BB.W1c (Sheets), BB.W2 (five-grammar cohort) close the all-grammar migration. BA.md §13-Lock honoured table marks Lock 1 as **deferred-with-receiver**, not honoured.

## §2 — Defence (i): Iter-time impact

| Surface | Scope | Iter-time |
|---|---|---|
| W2 god-module splits | 23 files | ~10s of moves; mechanical |
| W3 path-crate triplet | 3-crate rename + extract | ~5s `cargo check` |
| W4 cursor unification | 9 generated parsers regen | ~30s `cargo xtask regen --check` |
| W5 (option a) all-grammar direct-to-struct | 9 OpenFrame builders + 9 generated parsers | ~60s regen + 9× parity test runs |
| W5 (option b) JSON direct-to-struct | 1 OpenFrame builder + 1 generated parser | ~25s regen + 1 parity test |

Option (a) compounds 9 grammar migrations within the same tranche as 23 god-module splits, path-crate consolidation, and cursor unification. The cumulative iter-time on a per-wave `cargo xtask regen --check` ≥ 90s breaches BA-G3 (`cargo xtask regen --check` ≤ 30 s). Option (b) keeps the W5 regen surface to the JSON grammar's ~3,500 LOC → ~2,100 LOC delta (per `audit/MODULES-2026-05-03.md:621`); BA-G3 closer-gate at W6.M2 (regen ≤ 30 s) is achievable.

## §3 — Defence (ii): Regression-risk mitigation

Per the directive §10 honesty discipline ("No claim of lock-honoured if substrate is preserved"), option (a) — all-grammar migration in BA.W5 — would force CSS L4's 14-variant OpenFrame builder (1,014 LOC at `crates/core/src/runtime/css_l4/builder.rs`) to migrate to direct-projection IN THE SAME WAVE as the layout-lowering rename, the path-triplet consolidation, and the cursor unification.

Per the BA.md §Risks row 1 ("BA.W2 god-module splits cascade into BA.W5 codegen breakage (renames pull through emitter)"), the W2 splits land first; W5's regen comes last; xtask regen `--check` between waves verifies no cascade. Option (b) localises the high-regression-risk migration (CSS L4) to BB.W1a, where BB.W1a's W0 has no overlapping rename surface; the regression vector is mechanical (one grammar, one builder, one parity test) instead of cumulative (nine grammars, nine builders, nine parity tests).

## §4 — Defence (iii): Per-grammar test coverage

| Grammar | Pre-BA test surface | Post-W5 (option b) coverage | Receiving wave |
|---|---|---|---|
| JSON | `crates/core/tests/parse_with_json.rs` + bench `cargo bench --bench bench_json -- twitter` | BA.W5.M1+M2 (direct-projection emit + byte-disjoint Alt); BA-G1 + BA-G2 closer-gates | BA.W5 (in-wave consumer) |
| CSS L4 | `crates/core/tests/parse_with_css_l4.rs` + parity tests | OpenFrame retained; existing parity unaffected | BB.W1a |
| BBNF | `crates/core/tests/parse_with_bbnf.rs` + bootstrap parse | OpenFrame retained; bootstrap unaffected | BB.W1b |
| Google Sheets | `crates/core/tests/parse_with_google_sheets.rs` + sheets parity | OpenFrame retained; sheets parity unaffected | BB.W1c |
| BNF | `crates/core/tests/cohort_*.rs` | OpenFrame retained; trivial cohort unaffected | BB.W2 |
| CSV | `crates/core/tests/cohort_*.rs` | OpenFrame retained | BB.W2 |
| EBNF | `crates/core/tests/cohort_*.rs` | OpenFrame retained | BB.W2 |
| CSS Pretty | `crates/core/tests/cohort_*.rs` | OpenFrame retained | BB.W2 |
| Math | `crates/core/tests/cohort_*.rs` | OpenFrame retained | BB.W2 |

Option (b) preserves every grammar's test surface verbatim through BA close. Only JSON's parse_with test surface migrates (BA.W5.M1's regen produces a direct-projection-shape `crates/core/src/grammar/generated/json.rs`); the existing JSON parity assertions read against the new shape. The CSS L4, BBNF, Sheets, and cohort parity tests are unaffected at BA close — their regression risk routes to BB.W1a/W1b/W1c/W2 where the consumer migration lands in isolation.

## §5 — BA close Lock 1 disposition

Per BA.md §13-Lock honoured table:

| Lock | Wave | Disposition |
|---|---|---|
| L1. Tape + columnar dead | W0 (residue scrub); W5 (JSON OpenFrame retiral) | **Deferred-with-receiver**: CSS L4 → BB.W1a; BBNF → BB.W1b; Sheets → BB.W1c; cohort → BB.W2. The `enum OpenFrame` declarations at `crates/core/src/runtime/{css_l4,bbnf,google_sheets,bnf,csv,ebnf,css_pretty,math}/builder.rs` survive BA close. BB closes the all-grammar migration. |

The synthesis agent at BA close verifies the deferral receivers are real (BB.W1a/W1b/W1c and BB.W2 are drafted with closer-gates that name `rg -n 'enum OpenFrame' crates/core/src/runtime/<g>/` returning zero); per directive §10 honesty discipline, the carry is honoured by virtue of the receiving gate, not by claiming Lock 1 honoured at BA close.

## §6 — Closer condition

The decision is settled in-plan; option (b) is the BA close posture. BA.md §13-Lock honoured row L1 marks "Deferred-with-receiver: CSS L4 → BB.W1a; BBNF → BB.W1b; Sheets → BB.W1c; cohort → BB.W2."
