# SK-V8 S-P3 Hardening V2 CH3: Regression And Pre-Block Challenge

Date: 2026-05-18.
Pass: S-P3 Synthesis-Plan.
Cycle: V2.
Lens: CH3 REGRESSION.

## Scope

This challenge audits the V2 hardening fold and live packet for regression against REDRESS, the P3-E pre-block ledger, strict-vs-strict comparator discipline, typed/direct guard preservation, and behavior/status movement without row gates.

Inputs reviewed: `restart/prompts/ORCHESTRATOR.md` Section 3W/3Z, `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`, live `SPEC.md`, live `DISPATCH-PROMPT.md`, live `HANDOFF.md`, `restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md`, P3-A through P3-F, `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`, `skinny/RESULTS.md`, and `skinny/REDRESS.md`.

## Verdict

ACCEPT.

Confidence: 96%.

Blockers: none.

Required fold if REVISE: none; this review is ACCEPT.

## Evidence

### V2 Fold Scope

V2 folds the V1 CH1 and CH4 objections only: the SPEC now carries the W2 candidate typed seed table, W2 selection is constrained to that table unless a later accepted S-P3 revision expands it, future artifact references use naming patterns, and SPEC/DISPATCH/HANDOFF now carry per-wave LOC budgets plus a W3 pre-redress fit estimate (`restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:18`, `restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:25`). The fold explicitly preserves CH3 regression, changes planning constraints only, and does not authorize implementation or row-status movement (`restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:21`, `restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:24`).

V1 consolidation left CH3 as a qualifying ACCEPT at 95%, while V1 overall returned REVISE because CH1 and CH4 needed folds (`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:14`, `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:27`). The V1 accepted boundaries remain present: strict-vs-strict, Lock 14, W3 Tier A only, no `tape_vs_tape` production consumer, no sidecar/parser-owned cursor, no new directive/BIR/substrate surface, and no pre-blocked REDRESS reopen without fresh W0 evidence, same-wave consumer, no-regression gate, REDRESS citation, scalar/checkasm where relevant, and challenge acceptance (`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:54`, `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:69`).

### Pre-Blocked Routes

P3-E still enumerates the required CH3 route set: REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, plus historical function-pointer, pair-token, token-width, separator, generic SWAR, capacity prescan, EventCursor/sidecar, raw f64, and orphan primitive routes (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:14`). Its route ledger keeps those families blocked at route level, including Class A tiny-string, aux/EventCursor/parser-local cursor, direct materialization, raw f64, single-quartet Unicode, StringBlock16, object-pair value-byte, PMULL, CTZ, and B6 canary-as-performance routes (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:55`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:99`). Its per-wave checklist preserves W3, W4, W5, and W6 preblocks, including Tier A W3 blocking Class A tiny-string, EventCursor/sidecar/aux, retained wide-string scanners, Unicode classifier retries, object carry, `tape_vs_tape` consumer, and unconditional PMULL/CTZ (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:127`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:139`).

The live SPEC inherits the route ledger for every wave and permits reopening only with fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant, no-regression gate, REDRESS citation, and challenge acceptance (`restart/skinny/tranches/sk-v8/SPEC.md:767`, `restart/skinny/tranches/sk-v8/SPEC.md:771`). SPEC Section 10 carries the full specific list: REDRESS 16/17/18/25 historical routes, 28+33, 36-38/85-86, 49-55, 59-65 plus 72/83, 66-72 plus 80, 74-79/81/87 bounded evidence routes, 82-84, 88-90, Alpha-E bitmap reserve, and Tier B blocked from W3 Tier A (`restart/skinny/tranches/sk-v8/SPEC.md:786`, `restart/skinny/tranches/sk-v8/SPEC.md:812`). DISPATCH mirrors SPEC Section 10 and P3-E as pre-block authority and repeats the same required reopening package (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:164`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:184`). HANDOFF also preserves the required REDRESS and historical route list (`restart/skinny/tranches/sk-v8/HANDOFF.md:151`, `restart/skinny/tranches/sk-v8/HANDOFF.md:164`).

### Strict-Vs-Strict Discipline

No V2 fold loosens strictness. SPEC still classifies same-run strict anchors, flaw probes, and sidecar planning signals, and `gate-json` must reject strict admission when comparator plane, strictness, freshness, or measured-row validation fail (`restart/skinny/tranches/sk-v8/SPEC.md:61`, `restart/skinny/tranches/sk-v8/SPEC.md:77`). The non-negotiables preserve no strict admission except strict-vs-strict on a matching output plane and no stale sidecar, permissive, lossy, historical, or view-boundary evidence as strict admission (`restart/skinny/tranches/sk-v8/SPEC.md:230`, `restart/skinny/tranches/sk-v8/SPEC.md:251`). P3-D gives executable refusal rules for `row_output_plane != comparator_plane`, `Strictness != strict`, non-strict comparator, stale/sidecar-only evidence, non-measured validation, and `K`/`S` outcomes (`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:117`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:129`).

Current RESULTS still show the risk that strictness gates must block: every main row is `Strictness=deferred` and `parse_utf8=view-boundary`, with C++ comparators only sidecar planning signals when populated (`skinny/RESULTS.md:3`, `skinny/RESULTS.md:42`, `skinny/RESULTS.md:216`, `skinny/RESULTS.md:219`). V2 keeps these as opening telemetry, not admission evidence.

### Typed And Direct Guard Rows

The V2 SPEC now contains the W2 typed seed table and guard floors required by V1 CH1. It records the four current real-typed GO maintain floors, direct GO guard floors, and candidate typed seed floors, with the typed threshold rule `Track 1 Mbps >= ceil(sonic-rs strict Mbps / 1.10)` and post-W0 recomputation if the strict anchor changes (`restart/skinny/tranches/sk-v8/SPEC.md:144`, `restart/skinny/tranches/sk-v8/SPEC.md:189`). W2 entry constrains selected typed candidates to the Section 0.5 seed table unless a later accepted S-P3 revision expands it (`restart/skinny/tranches/sk-v8/SPEC.md:460`, `restart/skinny/tranches/sk-v8/SPEC.md:468`).

W2 exit preserves the direct/typed guard surface: at least two new generated typed rows must pass, the current `twitter`, `update_center`, `mesh`, and `marine_ik` real-typed rows must maintain GO and Section 0.5 floors, existing direct GO rows must maintain GO, non-target rows must stay within the -2.0% Track 1/Track 2 maintain budget, and Track 2/oracle coupling is forbidden (`restart/skinny/tranches/sk-v8/SPEC.md:478`, `restart/skinny/tranches/sk-v8/SPEC.md:489`). W2 preblocks hand typed sinks, hidden schema directives, direct digest as typed proof, capacity prescan, Track 2 coupling, retained/direct routes reopened through typed Vec admission, benchmark-private parsers, and generic JSON schema facts (`restart/skinny/tranches/sk-v8/SPEC.md:491`, `restart/skinny/tranches/sk-v8/SPEC.md:497`).

W4 keeps direct rows as guard rows rather than product-plane proof. Its selected rows must meet Track 1 and Track 2 floors, measured strict validation and sonic-rs strict anchor must be present, Track 2 must remain independent, non-target rows must stay within -2.0%, and existing direct GO and real-typed GO rows must maintain GO (`restart/skinny/tranches/sk-v8/SPEC.md:637`, `restart/skinny/tranches/sk-v8/SPEC.md:646`). W4 preblocks the REDRESS 54/55 and 66-72 repeat routes: sink-local decoded stats, quote-source streaming hash, direct source-hook folding, parser-owned scratch, byte-output unescape, semantic string facts for digest, raw f64 shortcut, stale mantissa widening, Track 2 coupling, direct cap-16 reruns, and digest as typed proof (`restart/skinny/tranches/sk-v8/SPEC.md:651`, `restart/skinny/tranches/sk-v8/SPEC.md:655`).

### Behavior And Status Regression Gates

V2 does not allow behavior/status movement without row gates. The global close condition requires any parse/direct behavior wave to meet named row thresholds and full-table maintain or reject with REDRESS, and prohibits pre-block reopen without the full evidence package (`restart/skinny/tranches/sk-v8/SPEC.md:40`, `restart/skinny/tranches/sk-v8/SPEC.md:59`). Required telemetry is gate-consumed, and missing fields, unsupported outcomes, strictness mismatch, stale sidecar, producer-only telemetry, W0 behavior drift, W1 CostFacts gaps, W3 side substrate, W3 telemetry substitution, Lock 14 generic leak, or cap overflow reject the wave (`restart/skinny/tranches/sk-v8/SPEC.md:138`, `restart/skinny/tranches/sk-v8/SPEC.md:142`).

W0 is telemetry-only and rejects parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, or generated parser output changes (`restart/skinny/tranches/sk-v8/SPEC.md:360`, `restart/skinny/tranches/sk-v8/SPEC.md:377`). W1 binds CostFacts and strict-admission refusal while preserving generated JSON output and parser behavior unless a separate challenged behavior consumer is accepted (`restart/skinny/tranches/sk-v8/SPEC.md:407`, `restart/skinny/tranches/sk-v8/SPEC.md:440`). W3 must cross post-W0 thresholds, keep all 38 rows within -2.0%, prove strict measured-row validation and admitted tape facts, preserve one retained tape, and keep `parse_only` substrate-guard unless a separate plane-matched strict gate is proven (`restart/skinny/tranches/sk-v8/SPEC.md:565`, `restart/skinny/tranches/sk-v8/SPEC.md:594`). W6 close rejects missing REDRESS, missing RESULTS rows, strict admission from sidecar/permissive evidence, PMULL/CTZ/B6 canary as performance evidence, architecture analogy without row data, and dropped falsifier rows (`restart/skinny/tranches/sk-v8/SPEC.md:743`, `restart/skinny/tranches/sk-v8/SPEC.md:765`).

### W3 Smuggling Check

W3 still does not smuggle Tier B, `tape_vs_tape`, PMULL/CTZ, sidecars, or parser-owned cursor/facts. P3-F states the W3 lead is Tier A structural-class cursor migration inside one retained `Tape`; Tier B string-boundary, quote/backslash/parity, CostFacts-template, `tape_vs_tape`, default PMULL/CTZ, and sidecar/parser-owned cursor routes remain blocked unless a future plan prices and challenges them separately (`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:31`, `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:36`). SPEC Section 6 matches this: Tier A does not claim string-boundary, quote/backslash/parity, CostFacts-template, non-JSON production, or direct/SinkOnly/path closure; representation replacement fails if retained beside old offset append or if a parser-owned cursor/fact slot survives (`restart/skinny/tranches/sk-v8/SPEC.md:551`, `restart/skinny/tranches/sk-v8/SPEC.md:563`).

W3's same-wave consumer remains generated JSON retained Track 1 parsing plus retained view/`ValueRef` as touched or proven-untouched; telemetry-only rows do not count (`restart/skinny/tranches/sk-v8/SPEC.md:584`, `restart/skinny/tranches/sk-v8/SPEC.md:586`). DISPATCH repeats the W3 consumer and blocked surfaces, including Tier B, `tape_vs_tape`, sidecar/aux/parser-owned cursor, old offset append, new substrate surface, `UnionTape`, `BackendShape`, directive, BIR, and public substrate API (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:127`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:140`).

## Non-Blocking Risks

1. Some local W3 entry language in P3-B/P3-C/SPEC names only a shorter REDRESS subset for the "not renamed" challenge line. This is not a blocker because SPEC Section 10 is explicitly inherited by every wave and DISPATCH names the full pre-block authority. Future W3 plans should cite SPEC Section 10 and P3-E directly.

2. HANDOFF summarizes REDRESS 50-55 as "SK-V5 UTF-8 fusion routes" (`restart/skinny/tranches/sk-v8/HANDOFF.md:156`, `restart/skinny/tranches/sk-v8/HANDOFF.md:159`). That wording is imprecise but non-blocking because SPEC, DISPATCH, and P3-E precisely name aux tables, EventCursor, parser-local cursor, decoded stats, and quote-source materializer routes.

3. P3-A/P3-B/P3-C/P3-D/P3-E still carry V1 cycle labels while P3-F and the V2 fold document identify the V2 fold. This is citation/governance hygiene, not a CH3 regression, because the live SPEC/DISPATCH/HANDOFF carry the folded gates and preblocks.

## Required Fold If REVISE

None. Verdict is ACCEPT.

## Self-Verdict

CH3 disposition: ACCEPT.

Confidence: 96%.
