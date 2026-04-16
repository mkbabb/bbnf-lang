# Tranche AP — Retrospective (R-AP)

## 1. Scope reality vs plan; deltas

AP declared six phases spanning correctness (0), structural activation
(1), payload enrichment (2), WS surgery (3), CSS demolition (4),
scanner surgery (5), calibration (6). Phase 0 landed cleanly
(bootstrap enum refs, `@ws` universality → tailwind parses, payload
f64 rewire, `css/monolithic.rs` delete). AP.2, AP.3.1, AP.3.3, AP.4.1,
AP.5.1–5.3 all landed — citm +44.7 %, twitter +41.1 %, tailwind FIX,
bootstrap +49.9 %, normalize +57.0 %.

**Flagship lever failed**: Phase 1 structural-dispatch activation
landed as AP.1b "synchronized peek-only" then was **gated OFF**
(`structural_mode = false`, `generate/mod.rs:61`) — pre-scan cost
(~4–5 ms on citm, dominated by scalar `filter_quote_parity`)
outweighed savings when WS-elision was refused.

## 2. Silent vs declared deferrals

**Silently dropped or gate-disabled:** AP.1 end-to-end activation;
AP.3.2 trim elision; AP.3.4 SIMD `filter_quote_parity`; AP.4.2 pattern
hoist (43×`ws:ws`, 42×`!important`); AP.4.3 CSS L4 type errors;
AP.4.4 CSS L4 structural investigation; AP.5.4 deferred UTF-8;
AP.5.5 `TapeBuilder` default prealloc; AP.6.4 cost-model grid sweep;
AP.6.5 global CSP solve. The plan's own line "**No deferrals.
Everything in this plan ships within the tranche.**" was violated.

**Declared OOS at plan-time** (correctly, with rationale): clean
bootstrap regen, 32-byte AVX2, padded-buffer mode, SIMD digit-int,
TaggedUnion boxing, TS/WASM migration, e-graph↔CSP bridge, PGO,
real struct-projection ABI.

## 3. Orchestration friction

Four agents × four waves ran without recorded inter-agent contention
— the 12-agent audit that preceded AP absorbed that cost. **No
FINAL.md, no PROGRESS.md** exists for AP; the only record is
`docs/benchmarks/post-AP.json` + git log. AP inherited AO as an
open tranche ("code complete, never exercised") without a close
ceremony — a pattern that then propagated into AQ and beyond.

## 4. Agent-layer friction

AP.1's failure is the central agent-layer lesson. Three specific bugs
(scalar `filter_quote_parity`; hybrid dispatch duplicates match arms
and always re-syncs cursor; checkpoint never saves
`structural_cursor` → backtrack desync) surfaced only in the AQ
audit. The agent landed a "synchronized peek-only" design whose
review missed the desync path.

## 5. Edict adherence

- *No deferrals*: **violated** (~10 sub-phases silently dropped).
- *No workarounds*: **violated** (`structural_mode = false` gate is
  exactly the "feature-flag-gate dead code" anti-pattern edict forbids).
- *Fix root causes*: partially — `@ws` universality fix was
  root-cause; structural gate-off was symptomatic.
- *Single-invocation bench sweep*: adhered to.
- *Honest naming*: adhered to — AP coined "tape + f64 side-channel
  + lazy cursor views" as honest replacement for "direct-to-struct".

## 6. Chronic deferrals IN + OUT

**IN (chronic)**: clean bootstrap regen (AC), global CSP (AL/AO),
cost-model grid sweep (AM/AO), Tier B (AF/AI/AM), scanner
generalization (AN), release instrumentation (AN.6/AO.4.3). **OUT**
(into AQ): all the above plus fresh
AP.1/3.2/3.4/4.2/4.3/4.4/5.4/5.5/6.4/6.5 drops.

## 7. Mid-tranche restructuring

AP.1 → AP.1b was a mid-tranche redesign ("synchronized peek-only")
that should, per the memory edict `new-tranche-new-doc`, have opened
a new tranche letter. Instead it renumbered in-place and still
ended gated-off. AQ then deleted the infrastructure wholesale
(Phase 5, ~400 LOC).

## 8. Lessons

1. **Activation is its own sub-tranche.** Every "build substrate
   → activate later" split across AO→AP→AQ ended with deletion.
   Infrastructure without a consumer in the same wave is dead on
   arrival. AW's W1 activation design flows directly from this.
2. **Gate-off is a deferral disguised as a commit.** A
   `feature_flag = false` landing masks an unfinished phase and
   dodges the edict against deferrals. Either the phase ships or
   the code is deleted — nothing in between.
3. **Post-hoc-only correctness review finds backtrack bugs too
   late.** The three AP.1b bugs (cursor desync, duplicated arms,
   scalar filter) were all discoverable by a `cargo expand`
   inspection in the wave itself. Expand-inspection must be a
   named pre-landing gate, not an audit-wave diagnostic.
