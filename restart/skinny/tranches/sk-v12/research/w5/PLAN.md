# SK-V12 W5 Plan: PASS-ADMIT Close Reconciliation

Date: 2026-05-21.

## Selected Disposition

Select `PASS-ADMIT` under `G-W5-CLOSE`.

The W1b-2b companion report already satisfies the USER PIN clause (a) ADMIT
predicate for `css_l4/declaration_values/direct_to_struct/main`: generated
Track 1 is `429.34420791225705 Mbps`, lightningcss same-plane strict comparator
is `168.92962215656692 Mbps`, the required threshold is
`169.92962215656692 Mbps`, the measured margin is `259.41458575569015 Mbps`,
and Track 1 / cssparser / lightningcss fact streams are strictly equal.

Per SPEC Section 10, W3 is not required because W1b-2b supplies an already
admitted CSS path. W4 supplies the required final orphan disposition and the
ASM-gen route record. This is not a `FIXPOINT` close.

## Owner Paths

W5 redress may edit only:

- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/SPEC.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md`
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`
- `restart/skinny/tranches/sk-v12/research/w5/`

No runtime, codegen, benchmark, SIMD, or gate source path is in scope.

## Redress Edits

1. Append REDRESS-127 closing W5 as `PASS-ADMIT`.
2. Append the CSS L4 row to both `skinny/RESULTS.md` tables and change the
   overall note from `N-direct / NoGo` to `A / Go`.
3. Relabel existing Track 1 / Track 2 notes as JSON-specific and add a CSS
   note pointing to the W1b-2b report.
4. Mark `SYNTHESIS.md`, `SPEC.md`, `HANDOFF.md`, and `DISPATCH-PROMPT.md` as
   closed/historical under `G-W5-CLOSE` / REDRESS-127.
5. Materialize `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`.

## Gate Boundary

The CSS row is consumed by the dedicated W1b-2b report gate
`sk-v12-css-l4-sota-v1`. The legacy `gate --check-results` path is JSON-row
shaped and must not be used to pretend the manually appended CSS row
round-trips through the JSON renderer.

W5 verifies the close by re-running:

- `RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --skv12-css-l4-sota-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json --advisory`
- `awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md`
- `jq -e '.decision == "pass" and .parity_status == "pass" and .candidate_speedup_ratio > .threshold_speedup_ratio' restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`
- `git diff --check`

## CHALLENGE Requirement

Run a six-lens CHALLENGE before redress despite W5 being docs/report-only,
because this plan changes the campaign close status and `RESULTS.md` outcome.
The CHALLENGE must accept:

- CH1 correctness: W1b-2b truly satisfies USER PIN clause (a).
- CH2 generality/Lock 14: close does not weaken grammar-neutral proof.
- CH3 regression/REDRESS: JSON guards, W4 orphan disposition, and REDRESS
  numbering remain coherent.
- CH4 cost: no expensive rerun is smuggled into W5; W5 reuses gate-consumed
  evidence.
- CH5 hidden coupling: W5 does not touch behavior/source/gate code.
- CH6 anti-paper-close: close docs point at consumed measurements, not prose.

## Revert Protocol

W5 is docs/report-only. Revert the W5 redress commit if any verification fails
or if CHALLENGE rejects the plan. No behavior patch exists.
