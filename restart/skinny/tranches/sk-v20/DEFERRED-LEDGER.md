# SK-V20 — The Deferred-Items Fold-In Ledger

Date: 2026-06-10. HEAD: `36ca10646`. Source: the 32-agent research pass (`research/05-deferred-census.md`
is the detailed census; the items below also absorb deferrals surfaced by the other 27 auditors). Per the
standing precept — never defer optimizations to future tranches — every item here receives a DISPOSITION
into the re-founded totality plan (`SPEC.md`); nothing is re-deferred without one.

## §1 — Chronic items (deferred 2+ cycles; the never-defer precept is violated structurally)

| # | Item | First seen | Cycles | State at HEAD | Disposition |
|---|---|---|---|---|---|
| C1 | AoS `TapeRec` → SoA `Tape` (exactly-one-encoding, Lock 1) | SK-V15 (REDRESS-53) | 4+ | OPEN — `tape/record.rs:103` 16-byte AoS is the sole core tape; unwired `TapeStructBuilder` is a latent dual-encoding trap | **TW1** (substrate landing, one fenced commit) |
| C2 | The C fold — ONE grammar-agnostic value-plane generator | SK-V17 (">SOTA burden") | 3 | OPEN — three emit bodies in skinny; zero `ValueRef` in core; `json_templates/` couriers alive | **TW2+TW3** (generator un-fork + value plane, co-landed with the cost model per D11) |
| C3 | BackendShape / decision engine in `crates/ir` | SK-V15 (MP.SK15.W7-9) | 5 | OPEN — `rg BackendShape crates/ir/src` = 0; skinny's engine is cost-theatre (perf_cost ≡ 0, tautological CSP) | **TW4** (one engine, measured KernelCostTable — not a port of the theatre) |
| C4 | Scanner unification (`crates/simd-scan` vs `bbnf-simd`) | SK-V18 (COH18-015) | 2 | OPEN — three competing classification schemes; 7 duplicate movemask copies | **TW4** (unify on lo/hi nibble two-LUT + single SHRN pack) |
| C5 | Lock-16 single-movemask co-gate | SK-V18 (LOCKS:641) | 2 | OPEN — ≥4 non-delegating `vaddv` pack bodies (the slow form) on hot kernels | **TW4** (vshrn pack + structural co-gate) |
| C6 | D-G pre-block reconcile (REDRESS 51/53/246/247) | SK-V19 SPEC §0.5 | 1 (blocks all substrate waves) | OPEN — no committed ledger entry; a hidden zero-LOC gate | **TW0** (first act — doc commit) |

## §2 — Open SK-V19 waves (the certified W0-W6 manifest, absorbed)

| SK-V19 wave | State | Disposition |
|---|---|---|
| W0 substrate_target classification | OPEN | **TW1** entry gate (re-anchored: the gate's strategy.rs:134-189 line-refs were deleted by W6a; key on `ManifestStrategyEntry`) |
| W1 eager-builder retirement | OPEN | **TW1** — corrected scope: THREE structural families (OpenFrame ×3 bespoke, Frame, SimpleStructBuilder over 5 grammars), not "11 families"; delete surface ≈ 1,080 hits not 47 |
| W2 AoS→SoA | OPEN (=C1) | **TW1** |
| W3 C fold + value-api derive | OPEN (=C2) | **TW2+TW3** |
| W4 StructRegistry projection fence | OPEN (needs W3) | **TW3** |
| W5a-c BackendShape + cost model | OPEN (=C3) | **TW4** |
| W5d x86/wasm production deletion in `crates/simd-scan` | OPEN — avx2.rs (370 LOC), avx512.rs, wasm.rs live behind cfg gates; plus `emit_support_module` stamps dead AVX2 bodies into all 9 generated grammars (~63 lines each) on an aarch64-only project | **TW0** (pure deletion, no behaviour change on aarch64) |
| W6a/b/c/e leak collapse + live Lock-14 gate | **DONE** (`f4633d468`, `36ca10646`) | Closed — but see §4 L1: the gate is green-by-exclusion |
| W6d StructLayout→Layout rename (960 hits/75 files) | OPEN | **TW1** tail (mechanical, after substrate settles) |

## §3 — Deferrals surfaced by the wider audit (newly ledgered)

| # | Item | Source | Disposition |
|---|---|---|---|
| N1 | `json_templates` Phase 3 retirement in skinny (the false-`@generated` couriers: value.rs 172, view.rs 488, visitor.rs 38, parser.rs 69, generated.rs preamble 391) | 12-json-courier | **TW2** — derive or own honestly; couriers are the W3 retire precedent |
| N2 | H1 quiet re-capture: ALL committed CSS absolute Mbps ran under loadavg 4.35–6.79; figures are directional only; G6-over-G2 delta never quantified quiet-clean | 14-bench-honesty | **TW0** (bench session, loadavg < 1.0, N=200, both sides) |
| N3 | JSON FRAMING.md: parse_only lane (validate-and-discard vs sonic IgnoredAny) conflated with typed lanes; warm-Criterion JSON vs cold CSS thermal mixing undisclosed at claim sites | 14-bench-honesty, 06-precepts | **TW0** (disclosure doc + co-located qualifiers) |
| N4 | Five grammars (CSV, Math, BNF, EBNF, CSS Pretty) have ZERO correctness oracle — only `parse().is_ok()` smokes | 15-oracle-census | **TW6** (oracle completion; CSV vs the `csv` crate first) |
| N5 | Dead/lying oracle surfaces: `color_channel_parity_rgb_family` returns early on empty (dead gate); named-color oracle keyed to the grammar itself not W3C; `eprintln!` soft-fail paths | 15-oracle-census | **TW0** (delete-or-activate; W3C rekey) |
| N6 | `css_l4.rs` generated file at 108,406 lines (7× budget), +27% with grammar source constant; PATH_PLAN literal arrays; `generated_loc_budget` read by zero code | 16-generated-size | **TW2** (emitter regression trace + enforced budgets in regen --check) |
| N7 | skinny `bbnf-regex` name-collision with the canonical parse-that engine; literal-string regex classification (`pattern == "[ \t\n\r]*"`); third ad-hoc FIRST scanner in general_descent | 17-parse-that-fold | **TW2** (skinny consumes the canonical engine via the same path patch totality uses) |
| N8 | Dead kernels behind a green checkasm: JSON `neon::scan` neutralized on live path (GrowOnly ⇒ const 256); `unescape_uxxxx_neon` zero callers; backend label lies (`ScanBackend::Scalar` stamped after NEON call) | 18-asm-simd, 19-dav1d | **TW4** (checkasm bound to liveness; wire-or-delete) |
| N9 | Type-system vaporware: the documented "rank-1 HM + bidirectional" does not exist; `-> map` bodies default to Span; `@host` signatures never typed; skinny has zero inference | 21-type-inference | **TW5** (honesty strike first, then the real core) |
| N10 | Value API: three substrates, four JSON-pointer-shaped path APIs, dead `ser::Deserializer` with hardcoded JSON delimiters | 22-value-api | **TW3** (one substrate, one derived path alphabet, serde bridge over the lazy cursor) |
| N11 | Lock violations standing in the totality tree: Lock 4 (egraph→csp direct dep), Lock 15 (lto thin, no panic=abort), Lock 12 (ser/gorgeous unarchived), Lock 6 (regen fork) | 02-masterplan-locks | **TW0** (Lock 4/15 are two-line fixes; Lock 12 archive decision; Lock 6 = TW2) |
| N12 | Inline `#[cfg(test)]` in src/: 7 core + 2 skinny violations, count not trending down | 06-precepts | **TW0** (mechanical relocation) |
| N13 | Unused language surface: closures/GrammarCall/multi-param fns + `@host` directive used by ZERO production grammars (the real host path is metadata TOML); `^`→Right associativity hardcoded | 25-grammar-language, 11-ir-residual | **TW6** (land a consumer or delete; `@assoc` directive) |
| N14 | CSS depth closure: recognizer emits only 3 structural tape kinds; the grammar already specifies the full typed value tree and codegen discards it; stale "equal-depth" claim in H1-LEDGER:11-13 contradicts FRAMING.md | 26-depth-parity | **TW3** (additive value-component tape events; ledger correction in TW0) |
| N15 | Full-tree-touch bench plane (where laziness stops winning) does not exist | 26-depth-parity | **TW0** bench harness addition (honesty bound for the lazy claims) |
| N16 | MASTER-PLAN receiver-row drift: SK-V14..V18 rows read "active pending" though cycles closed; SPEC counts stale (corrected in §2) | 02, 28 | **TW0** (one-time reconcile) |
| N17 | `bbnf-path`/`bbnf-path-ts` registry promise: proc-macro still reads a JSON sidecar via std::fs; only bbnf+sheets wired | 22-value-api | **TW3** |
| N18 | Empty-husk crates: skinny `simd-scan` (no .rs files); `bootstrap` vestige (465 LOC); `analysis` consolidation unexecuted | 23-crate-topology | **TW1/TW2** topology moves |

## §4 — Items NOT folded (deferred by design, with authority)

| Item | Authority | Why it stays deferred |
|---|---|---|
| TS/WASM backends | Lock 5 (post-V1) | The Emitter seam must survive in the design (TW2), but no TS/WASM implementation work in V1 |
| Profile-guided specialization | deferral-audit-6 §4 | Requires the measurement loop TW4 builds; revisit at TW4 close |
| TS path runtime (bbnf-path-ts full) | deferral-audit-5 §3 | Post-V1, rides the WASM decision |

**L1 — standing caveat carried into every TW gate:** the live Lock-14 gate at HEAD is GREEN for a stub
system — it scans only `crates/ir` + `crates/analysis`, with a hand-maintained 17-token roster, while
`crates/core` (ten hardwired grammar modules, per-grammar runtime dirs), `xtask` (the regen fork), and the
skinny codegen surfaces sit outside the scan boundary. The TW0 gate-widening (roster derived from workspace
metadata; all generic crates scanned; per-surface re-inject falsifiers) is what makes later GREENs meaningful.
