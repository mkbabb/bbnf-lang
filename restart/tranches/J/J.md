# Tranche J — Cross-backend Parity + Close

## Gestalt

Tranche J is the close ceremony. The cross-backend parity matrix lands — every grammar produces identical typed-tree output across Rust, TS, WASM (the parity matrix already proven in tranche H lifts to a comprehensive integration test fixture). The final perf gates per Lock 8 fire: sonic-rs M1 Pro twitter ≤ 436 µs; lightning-css Bootstrap ≤ 4.16 ms; simdjson On-Demand 7 GB/s. SOTA validation reports land per gate at `audit/restart/perf-2026-XX-XX-<grammar>.md`.

The Lock 14 onboarding test (introduced in tranche E.W7) re-runs as part of close — adding a synthetic 11th grammar via metadata + declaration crate + regen produces a working parser with ZERO code change in any other crate. The new-grammar onboarding gate is the substantive honour-test for Lock 14; passing it post-J close means the substrate is grammar-generalised by construction.

The cross-tranche debt reconciliation lands per the master plan §13 R24 — every deferred item from tranches A-I has either landed in its receiving tranche or has its disposition documented at the close. The `FINAL.md` per tranche cites every gate's evidence; the master plan's hardening close fires; the restart suite closes.

The 1.0 release prep lands as the final wave: version bump across all crates, crates.io publication of `bbnf` (the user-facing aggregator), and `bbnf-cli` activation per Pass C facility §4.5 (deferred from earlier tranches; now lands as a 1.0 deliverable).

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| Cross-backend parity matrix complete | J.W0 | Rust ↔ TS ↔ WASM produce identical typed-tree on every grammar's test fixture; per-grammar parity at `tests/cross_backend_parity.rs` |
| sonic-rs twitter perf gate | J.W2 | JSON parse on twitter.json ≤ 436 µs on M1 Pro per Lock 8; per-platform numbers documented |
| lightning-css Bootstrap perf gate | J.W2 | CSS L4 parse on bootstrap.css ≤ 4.16 ms; per-platform numbers documented |
| simdjson On-Demand 7 GB/s perf gate | J.W2 | JSON parse throughput ≥ 7 GB/s per Lock 8; per-platform numbers documented |
| Lock 14 onboarding test (synthetic 11th grammar) | J.W3 | adding new grammar produces working parser with zero code change in other crates |
| Cross-tranche debt reconciliation | J.W4 | every deferred item from A-I landed in receiving tranche; remainder dispositioned at close |
| 1.0 release ceremony | J.W4 | version bump across all crates; `bbnf` aggregator published; `bbnf-cli` activates |
| All 14 locks honoured at greenfield completion | J.W5 | master plan §11 verification commands all pass |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| J.W0 — Cross-backend parity matrix | comprehensive integration test: every grammar × Rust/TS/WASM | 4 parallel (per-cohort batches) | parity matrix passes; per-grammar test fixtures land |
| J.W1 — Pre-perf-gate setup + bench harness final | bbnf-bench harness final touches; SOTA-anchor data fixtures (twitter.json, bootstrap.css, etc.) baselines | 2 parallel | harness operational; baseline data landed |
| J.W2 — SOTA-anchored perf gates | sonic-rs + lightning-css + simdjson gates fire per grammar | 3 parallel (per gate) | gates pass or fail with documented evidence |
| J.W3 — Lock 14 onboarding test + cross-tranche debt audit | synthetic 11th grammar onboards; cross-tranche debt list reconciles | 2 parallel | onboarding gate passes; debt list closes |
| J.W4 — `bbnf-cli` activation + 1.0 release prep | bbnf-cli substantive impl per Pass C facility §4.5; version bumps; publication | 2 parallel | bbnf-cli works; aggregator publishes |
| J.W5 — All-14-locks honour audit + close ceremony | full audit per master plan §11 verification commands; FINAL.md per tranche; restart suite close | 1 | all gates pass; FINAL.md lands; suite closes |

## Carry-tags FROM

| Carry | Source tranche | Gate |
|---|---|---|
| All preceding tranche outputs | A-I | (continuous; J synchronises) |
| First SOTA-anchored perf gate | F | F.W6 |
| TS + WASM emitters integrated | H | H.W2 |
| `parse / parse_in / parse_owned` API + pointer macro | G | G.W2 |
| Published sister crates | I | I.W2 |
| 9 per-grammar declaration crates | E | E.W3 |
| Direct-projection emit (OpenFrame retired) | E | E.W4 |
| Lock 14 onboarding test (initial) | E | E.W7 |

## Carry-tags TO

(none — J is the close)

| Carry | Disposition |
|---|---|
| Restart suite closes | `audit/restart/FINAL-2026-XX-XX.md` lands |
| 1.0 release ceremony | crates.io publication of `bbnf` aggregator |
| Pre-restart-2026-05-03 tag preserved | (continuous; commit-chain provenance honoured) |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | substantively-honoured | (continuous from E; J.W5 audit confirms) |
| 2 — Layout canon | substantively-honoured | (continuous from C; J.W5 audit confirms) |
| 3 — Cursor + byte-skip | substantively-honoured | (continuous from E; J.W5 audit confirms) |
| 4 — Per-domain orthogonal | substantively-honoured | (continuous from F; J.W5 audit confirms) |
| 5 — IR + per-backend | substantively-honoured | (continuous from D; J.W5 audit confirms) |
| 6 — xtask source emit | substantively-honoured | (continuous from E; J.W5 audit confirms) |
| 7 — `crates/path/` consolidated | substantively-honoured | (continuous from C; J.W5 audit confirms) |
| 8 — Surpass SOTA | substantively-honoured | J.W2 (final perf gates fire per Lock 8) |
| 9 — Slice-borrow primary | substantively-honoured | (continuous from G; J.W5 audit confirms) |
| 10 — Pratt + SIMD auto-detected | substantively-honoured | (continuous from F; J.W5 audit confirms) |
| 11 — Path-deps for sister crates | substantively-honoured | (continuous from I; J.W5 audit confirms publication) |
| 12 — ser + gorgeous archive | substantively-honoured | (continuous from A; J.W5 audit confirms) |
| 13 — No god directories | substantively-honoured | (continuous from C; J.W5 audit confirms) |
| 14 — Full grammar generalisation | substantively-honoured | J.W3 (onboarding test passes) + J.W5 audit |

All 14 locks honour at greenfield completion. The audit at J.W5 fires the master plan §11 verification commands per crate.

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| Cross-backend parity matrix reveals divergence | J.W0 staged: per-grammar parity test fires; divergence triggers triumvirate (per-grammar narrow scope; not blocking other tranches) |
| SOTA-anchored perf gate fails (sonic-rs twitter > 436 µs) | J.W2 instrumentation: samply profile; if hardware-bounded, document the gap with platform-specific number; if substrate-bounded, redress via triumvirate; potential carry to post-J 1.x perf push |
| Lock 14 onboarding test fails on synthetic 11th grammar | J.W3 narrow scope: identify hidden coupling; per master plan §13 R4 + §13 R20; triumvirate if substantive |
| Cross-tranche debt list incomplete | J.W4 audit traverses every tranche's `FINAL.md`; deferred items reconciled or dispositioned-with-rationale |
| `bbnf-cli` substantive impl introduces unforeseen API drift | J.W4 staged: bbnf-cli is thin clap wrapper around bbnf substrate; no new API surface |
| 1.0 release ceremony reveals breaking change in published sister crates | J.W4 audit: cross-crate version compatibility check post-publication |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| Cross-backend parity test runtime | ≤ 10 min total | J.W0 |
| Per-grammar SOTA gate run | ≤ 5 min per gate | J.W2 |
| Onboarding test (synthetic grammar) | ≤ 10 min | J.W3 |
| Generated-LOC budget | J.exit: 173,750 LOC (unchanged from I.exit) | per master plan §12.2 |

## Voice locks

Per master plan §14. Tranche J's prose register: unpretentious-academic with mild lilt at ~5%; the close ceremony invites narrative weight ("the substrate's identity is settled"; "the 14 locks honour entire"); domain verbiage from compiler theory + Romantic-era musical idiom (the close has cadential character).

## Closing posture

Tranche J closes with the restart suite complete. All 14 locks honour at greenfield completion; the cross-backend parity matrix passes; the SOTA-anchored perf gates fire per Lock 8; the Lock 14 onboarding test confirms grammar-generalisation by construction. The 1.0 release ceremony lands; the `bbnf` aggregator publishes; the restart's substrate identity is settled.

Hereupon the master plan's authority discharges. The substrate is built; the locks are honoured; the precepts are observed; the greenfield mandate is satisfied — no quick solutions, no workarounds, no carry-forward of legacy substrates except by explicit ratification per file. The 2,621 commits of the prior chain remain accessible via `git checkout pre-restart-2026-05-03`; the post-restart chain begins at the prelude commit on `master-greenfield-2026-05-03`.

The plan from tranche J is the user's. The restart's ratification is total.
