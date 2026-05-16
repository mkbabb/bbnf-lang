# Handoff SK-V7

Date: 2026-05-16.

Status: **SK-V7 spec materialized. Wave 0 ready for dispatch.**

The implementation agent's reading order:

1. `restart/prompts/README.md` (framework gestalt).
2. `restart/prompts/ORCHESTRATOR.md` (phase identification + dispatch matrix).
3. `restart/prompts/PASS-ALPHA.md` (this packet's contract).
4. `restart/prompts/SKINNY-PASSES.md` (per-wave triumvirate).
5. `restart/skinny/audit/GRAND-SYNTHESIS-SK-V7.md` (the why).
6. `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V7.md` (the what + when).
7. `skinny/RESULTS.md` (current gate).
8. `skinny/REDRESS.md` (rejected-route ledger).
9. `restart/skinny/audit/SK-V7-COHORT/` (18 cohort reports).

## 1. Where SK-V7 Stands

Post-V6 measured state (per `skinny/RESULTS.md`):
- Parse: 13 of 17 rows G/NO-GO; 4 PASS (canada, mesh, marine_ik, numbers
  at A/GO).
- Direct: 11 N-direct; 6 PASS (citm, apache_builds, github_events,
  instruments, mesh-borderline, numbers).
- real_typed_struct: 2 PASS (twitter 151.5% sonic, update_center 99.2%).
- sonic-rs comparator is utf8_lossy flaw probe (per A1).
- yyjson 1.98x gap on twitter is the largest M5 Max deficit.

V7 cohort (18 reports archived) surfaced:
- B1 per-`\uXXXX` hypothesis applies to only 2 of 4 named rows (C1
  critical correction).
- twitter 151.5% real_typed_struct is from skip-work, not faster parse
  (C3).
- mesh DirectBuild blocked by codegen Vec helper shape-blindness
  (C3); needs DirectTypeRef::Vec specialisation FIRST.
- Eisel-Lemire only 5.2% of mesh cost (C2); digit scan dominates;
  ~25% of canada f64 overflow EL fast path.
- Lock 1 honored; Lock 14 violated at 46 HIGH leaks (A5).
- CostFacts substrate absent — REDRESS 72 empirical proof (A6).
- Lock 15 PASS with 42-58% headroom (C4).

## 2. Wave Dispatch Posture

Per IMPLEMENTATION-PACKET-SK-V7.md §2 through §12:

| Wave | Scope | Hard cap | Predicted close |
|---|---|---|---|
| **W0** | sonic-rs strict rebuild (one-line Cargo) | 60 min | instruments + unicode_basic flip toward PASS |
| **W1** | TapeKind rename (Lock 14 lowest-risk) | 90 min | -3 Lock 14 HIGH; no row regress |
| **W2** | B5b EL mantissa widen + canada fallback elim | 105 min | canada direct PASS |
| **W3** | B5 mesh DirectBuild + DirectTypeRef::Vec specialisation | 165 min | mesh + marine_ik typed PASS |
| **W4** | B1 per-`\uXXXX` TBL (per-quartet, not 4-batch) | 125 min | unicode_escapes + y_string_unicode lift |
| **W5** | B2 NEON 16-byte plain-string scan | 165 min | 4+ string-bound rows lift |
| **W6** | B6 control/key compaction | 165 min | citm + instruments lift |
| **W7** | Lock 14 Phase A+B (parse-that-regex + passes) | 240 min | -20 Lock 14 HIGH |
| **W8** | Lock 14 Phase C+D (codegen + ir) | 360 min | -38 Lock 14 HIGH total |
| **W9** | CostFacts substrate (B2 design) | 360 min | substrate landed; 7 rules cost-bound |
| **W10** | bbnf.asm bodies (PMULL + CSSC CTZ) + B6 hardening Stage 1 | 240 min | 2 primitives admitted; checkasm hardened |

Total: ~38 hours wall-clock across 11 waves; ~33 commits (3-4 per
wave via triumvirate).

After W10, dispatch Pass Alpha for SK-V7→SK-V8.

## 3. Pre-Blocked Routes (DO NOT RE-OPEN)

Per SK-V6 REDRESS entries 50-72 + SK-V5 entries 50-55 + SK-V4 entries
and earlier. The full ledger is at `skinny/REDRESS.md`. Key entries:

- REDRESS 50-55: 5 SK-V5 UTF-8 fusion routes (parse-time retained
  projection, byte-class whitespace cursor, parser-local structural-
  mask cursor, exact decoded-string stats sink, quote-source fused
  streaming materializer).
- REDRESS 60-72: 7 SK-V6 retained-parse + direct-materialization
  routes (delayed-wide retained string scan, retained Unicode-escape
  run validator, object next-key carry, global tiny-string cap,
  hand-authored real typed struct sink, direct source-hook field-
  layout materializer, parser-owned decoded scratch, byte-output
  unescape, DirectBuild semantic string facts).
- REDRESS 28+33: Class A NEON tiny-string wiring as parse-G fix
  (twice-rejected).

Also pre-blocked from earlier tranches:
- 12-byte token width churn (REDRESS).
- Pair-token fusion (REDRESS).
- Function-pointer dispatch table (REDRESS).
- Capacity prescan (REDRESS).
- Generic SWAR whitespace skipper (REDRESS).
- Separator elision (REDRESS).
- Raw f64 shortcut (REDRESS).
- PSI/DTA Rust-codegen automaton (V9.5 PSI excavation).
- EventCursor parallel prepass (REDRESS; substrate-without-consumer).

## 4. Entry Gates Per Wave

Each wave begins by re-reading: GRAND-SYNTHESIS-SK-V7 §6 + the SK-V7-
COHORT report relevant to the wave's domain.

| Wave | Entry gate | Source authority |
|---|---|---|
| W0 | This packet committed | this packet |
| W1 | W0 closed; RESULTS schema v3 populated | wave-0-strict-baseline.md |
| W2 | W1 closed; cargo test green; byte-identical generated.rs | wave-1 commits |
| W3 | W2 closed; canada direct PASS confirmed | wave-2 REDRESS entry |
| W4 | W3 closed OR parallel-safe per cohort guidance | wave-3 REDRESS entry |
| W5 | W4 closed (B1 per-quartet TBL admitted) | wave-4 REDRESS entry |
| W6 | W5 closed (B2 NEON 16-byte admitted) | wave-5 REDRESS entry |
| W7 | W6 closed OR parallel-safe with W2-6 | wave-6 + Lock 14 audit |
| W8 | W7 closed (parse-that-regex + passes Lock 14 done) | wave-7 commits |
| W9 | W8 closed (codegen + ir Lock 14 done) | wave-8 commits |
| W10 | W9 closed (CostFacts substrate landed); same-wave OffsetTape consumer wired | wave-9 commits |

## 5. Exit Condition (SK-V7 Close)

ALL must hold simultaneously:
1. `skinny/RESULTS.md` schema v3 columns populated (24 columns per
   PASS-ALPHA §4.3).
2. Strict-vs-strict comparator gate: sonic-rs strict rebuilt; per-row
   Mbps recorded.
3. ≥6 parse rows PASS at ≥100% strict sonic.
4. ≥10 direct rows PASS.
5. ≥3 real_typed_struct rows PASS (mesh + marine_ik + canada via W3).
6. Lock 14 HIGH leak count: -38 (≥-83% reduction).
7. Lock 15 PASS confirmed on post-W5/W10 baseline.
8. CostFacts substrate landed; 7 JSON rules cost-bound.
9. 2 new bbnf.asm primitive bodies admitted (PMULL + CSSC CTZ) with
   same-wave consumers.
10. Twitter parse hard residual documented + V8 fusion-refactor route
    named in HANDOFF-SK-V8 (via Pass Alpha).

## 6. The Hard Residual (Twitter Parse)

The yyjson 1.98x gap on twitter is structural: yyjson achieves 3,687
MiB/s via one fused scalar driver + force-inline + single-pass scan
(Lock 15 fusion-quality discipline). bbnf-skinny at ~1,950 MiB/s is
half. No single kernel intervention closes this.

The route to closing:
- Move `parse_value_at` to a single fused LLVM-target driver.
- Mirror yyjson's i-cache discipline (currently bbnf has 46% headroom
  per C4; usable).
- Possibly: collapse the lower/sink_only.rs path into the
  lower/offset_tape.rs path so the typed product plane and the
  retained parse plane share one driver.

This is V8 scope per Pass Alpha SK-V7→SK-V8 dispatch. Not W11+ of V7.

## 7. The Pass Omega Trigger

After SK-V7 close, the user may dispatch `dispatch omega` to fold
SK-V7 lessons into V1 spec amendments + locks proposals. Per V7
GRAND-SYNTHESIS §9, candidate amendments:
- Lock 1 strengthening (parallel-substrate forbidden in every shape).
- Lock 14 strengthening (CI grep-gate on generic crates).
- NEW LOCK (proposed): no bench-private Track 1 / no Track 1 ≡ Track 2 dishonesty.
- NEW LOCK (proposed): comparator-plane strictness disclosure mandatory.

Pass Omega CRUD agents will execute these as V1 spec edits after
G-Omega user sign-off.

## 8. The Triumvirate Discipline

Every wave produces 3-4 commits (per SKINNY-PASSES §9):
1. `docs(sk-v7-wave{W}-research): archive {scope} cohort reports`.
2. `docs(sk-v7-wave{W}-plan): select {intervention name}`.
3. `feat(sk-v7-wave{W}): admit {intervention name}` (on success) OR
   `docs(sk-v7-wave{W}-redress): reject {intervention name}` (on
   failure, with measurement evidence).

Optional 6-lens CHALLENGE between phases for high-risk waves (W2, W3,
W5, W9).

No commit merges roles. No wave closes without REDRESS entry. No
primitive ships without scalar reference + checkasm parity +
same-wave consumer.

## 9. Status Discipline

The orchestrator (or implementation agent) emits status ticks every
~5 min of silent wait. Tick format per ORCHESTRATOR.md §11:

```
[V{V} sk-v7-W{W}] {N} agents in flight; {M} returned; {K} pending CHALLENGE; ETA {time}
```

## 10. Closing Posture

SK-V7 is the **first cycle of the iterative auto-convergent
framework** (`restart/prompts/`). The Pass Alpha output is this
packet. The Pass Omega trigger awaits SK-V7 close.

The architecture is correct. Lock 1 holds. Lock 14 has known debt with
named cleanup. Lock 15 holds with headroom. Lock 16 vocabulary grows
monotonically.

The discipline is the suite:
- Triumvirate per wave.
- Same-wave consumer rule.
- Strict-vs-strict comparator gate.
- Telemetry-bound goalset (PASS-ALPHA §4.3 schema).
- 6-lens CHALLENGE for high-risk waves.
- Hard caps + revert protocol.
- Pre-blocked routes from REDRESS.

**Dispatch Wave 0 of SK-V7.**
