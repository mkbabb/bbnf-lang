# AUDIT-B — AY-II.W0' Close Ceremony Viability Under the Bootstrap Wall

**Date**: 2026-04-25
**Auditor**: AUDIT-β (deep audit)
**Scope**: read-only viability assessment of `docs/tranches/AY-II/waves/W0p.md` §Orchestrator-owned close ceremony against the post-W0'.d3 substrate
**Inputs**: AY-II.md, W0p.md, PATH-FORWARD.md, PROGRESS.md, audit/W0p-{regen,infra}-{root-cause,fix-plan}.md, waves/W1.md–W5.md, scripts/bootstrap-bbnf.sh, crates/derive/src/lib.rs

## 1. Headline verdict

**The W0p.md ceremony as written is partially viable on current hardware — but the §Orchestrator-owned close ceremony is over-spec'd for what W0' actually closes.** Two ceremony steps are load-bearing on the AY-II thesis (cycle-1 regen + a single fresh expand to verify FusedBuilder symbols emit). Three are **theatrical at W0' close**: cycle-2 cache-cleared idempotency duplicates a content-keyed cache invariant (`crates/derive/src/lib.rs:147` — `BBNF_SCHEMA_VERSION` + grammar contents → cache key), the fat-LTO 5-bench matrix re-pays a wall W1 will republish anyway (W0p.md:255-260 — orchestrator's own admission), and samply across four primary grammars at W0' provides baselines that no W0' invariant reads. Total wall under the as-written ceremony is bounded above by ~2× cycle-1 + 4× expand + 5× fat-LTO bench + 4× samply — empirically `> 4 hours`, against an honest W0' floor of `~10–15 min` (one cycle-1 regen at the post-d3 baseline).

The d3 fix (`f768f50d`) restored the value-side O(N²) regression to O(1) (`audit/W0p-regen-fix-plan.md:182-197`), but the prior dispatch surfaced an orthogonal infra wall — gorgeous-as-dev-dep + 5× serial `#[derive(Parser)]` inside one rustc = 9:16 cold (`audit/W0p-infra-root-cause.md:16,40`). B1 closed with the d4–d7 dev-loop infra patches landed (`PATH-FORWARD.md:24-28`), bounding the routine `iter-check` cold to 11.3 s (`PROGRESS.md:340-348`). The 80+ min cold expand the user described is consistent with a pre-d3 + pre-d7 cold path; the post-B1 regen wall is materially smaller, but not zero — `cargo expand -p bbnf-bootstrap --lib` still drives one full 17-pass + codegen invocation against the BBNF grammar, which on the post-d3 substrate is the 3–6 min historical baseline (`audit/W0p-regen-root-cause.md:181-187`).

## 2. Per-step wall-payment trace

| Ceremony step (W0p.md §Orchestrator-owned close ceremony) | Pays bootstrap wall? | Per-invocation cost (post-d3, post-B1) | Verifies |
|---|---|---|---|
| 1. `bash scripts/bootstrap-bbnf.sh` (cycle-1 regen) | **YES** — drives `cargo expand -p bbnf-bootstrap --lib` (script:44), which forces `bbnf_derive::bbnf_derive` (derive/src/lib.rs:281) to run the 17-pass pipeline + `generate_all` against the live BBNF grammar | 3–6 min historical (`audit/W0p-regen-root-cause.md:181`); post-d3 path stays in this band | Invariants 14–19 only structurally — the regen produces a `generated.rs` that is the artefact those invariants test against |
| 2. Cycle-2 (cache-cleared, then `diff`) | **YES** — clearing `.bbnf-cache/` and re-running pays the SAME wall as cycle-1 (the `if let Some(cached) = read_cache(key)` short-circuit at derive/src/lib.rs:300-303 cannot fire after cache-clear) | 3–6 min repeat | Invariant §12 ("Bootstrap regen cycle-1 = cycle-2 byte-identical") — but see §4 |
| 3. Fresh expands × 4 grammars (`cargo expand -p bbnf --bench {json,css_l4,sheets,bbnf}_monolithic`) | **YES — 4×.** Each `cargo expand --bench` forces a fresh derive expansion of every `#[derive(Parser)]` site in the bench's transitive graph. Bench graphs include the bbnf core lib (which contains `generated.rs` for BbnfBootstrap, but the grammar deriving in the bench is a separate `#[derive(Parser)]` on `JsonParser`/`CssL4Parser`/etc.) | Each expand: 1× full pipeline run for that grammar (per-grammar ~30k LOC TokenStream emit) — empirically tens of seconds to minutes per derive site at -O0 (`audit/W0p-infra-root-cause.md:36-37,75`) | Invariant §16 (materializer call sites in `project_value_*`), §17 (`STRUCTURAL_SCAN_POLICY` consumers), and `cargo expand` evidence per AY-II.md §Operational posture 4 |
| 4. fat-LTO 5-bench matrix (`make ay-bench-close WAVE=W0p-mid`) | **YES, MULTIPLE TIMES.** `cargo bench --profile bench` invokes a full fat-LTO + DWARF link of EACH bench binary (Cargo.toml `[profile.bench]` inherits the workspace bench profile, set to fat LTO by default on bbnf). Each bench binary links bbnf, which transitively pays the proc-macro derive expansions — but the cache short-circuits if grammar/schema unchanged (derive/src/lib.rs:300-303). The fat-LTO link itself is the headline cost. | Per bench: 1–10 min link (fat LTO over a 33k-line `generated.rs` + 4× per-grammar derives + dependents) × 5 benches | Hard gate §8 (`make ay-bench-close WAVE=W0p-close` clean) — but W0p.md:255-260 self-admits "Fat-LTO publish-grade peer-parity numbers ... capture at AY-II.W1 close" — i.e. the W0' fat-LTO is ALREADY downgraded in spec to `profiling-prep` (thin LTO), see step 4 ambiguity below |
| 5. samply per primary grammar | **NO — uses prebuilt binary.** `make ay-samply-json-twitter` requires `CARGO_TARGET_DIR/profiling-prep/deps/json_monolithic-*` (Makefile ay-samply target), which `make ay-prepare-profile-wave` produces via `scripts/prepare-profile-wave.sh`. The prepare step pays its own wall once. samply itself is a runtime profiler: pre-built binary + `samply record` → no proc-macro re-expansion. | Per fixture: ~30s–2 min runtime profile (record + symbolicate). The `ay-prepare-profile-wave` precondition pays one bench-binary build. | Invariant §3 (W0'-era retired symbols absent — but `nm` proves this directly), no specific W0' invariant ties to samply baseline content |
| 6. `nm` on each bench binary | **NO** (consumes step 5's prebuilt binaries) | Seconds | Hard gates §1, §2 (ValueBuilder/push_compound absent — but `rg` at the source level already proves this; `nm` only adds "absent in compiled artefact"); invariant §3 (`note_push` absent) |
| 7. PROGRESS.md update | **NO** | Negligible | Bookkeeping |

**User's hypothesis was correct**: the 5-bench matrix does NOT directly pay the bootstrap wall on each bench — the bench binaries link the pre-compiled bbnf lib whose `generated.rs` is checked-in source. What the bench matrix DOES pay is fat-LTO link time × 5, which on a 33,293-line `generated.rs` (`wc -l crates/core/src/grammar/generated.rs` = 33293) is itself 1–10 min per binary even before any proc-macro work.

The expansive walls are: (a) cycle-1 + cycle-2 each pay one full BBNF derive (ratio 2:1 of identical work — see §4); (b) the four `cargo expand --bench` calls each pay a fresh derive expansion against the bench's grammar AND that bench's transitive dev-deps if any (`gorgeous` is no longer transitively pulled per d4–d7); (c) the fat-LTO link cost × 5.

## 3. Invariant verification audit

Mapping ceremony steps to which AY-II.md / W0p.md invariants they actually verify.

| Ceremony step | Invariant(s) it verifies | Already verified by source landings + compile gates? |
|---|---|---|
| Cycle-1 regen | None directly. Produces the artefact later steps test. | The W0'.a/b/c source landings + `cargo iter-check-full` already prove the substrate compiles; cycle-1 verifies that the source's emitter changes haven't broken expand-time emission |
| Cycle-2 idempotency (cache-cleared) | AY-II.md §12 ("cycle-1 = cycle-2 byte-identical") | **PARTIALLY.** Cache-keyed determinism is a property of the proc-macro cache (`compute_cache_key` at derive/src/lib.rs:128-175 hashes BBNF_SCHEMA_VERSION + grammar contents + attrs + ident). What cache-cleared cycle-2 actually tests is: "does the pipeline produce a deterministic TokenStream from the same inputs?" That is a property of `bbnf::generate::generate_all` — and is more strongly tested at **W4 close** under self-hosting (W4.md:16). At W0' the harder cycle-2 question (parser-rewrite identity) doesn't apply yet — `generated.rs` is the BBNF grammar from a NON-self-hosted parser path, so cycle-2 here only tests pipeline-determinism, not self-hosting identity |
| Fresh expands × 4 | W0p.md §16 (materializer call sites), §17 (scan-policy splice), AY-II.md §Operational posture 4 | **NO** — these are essential. `rg` at the source level cannot prove that `quote!` emission produces the expected runtime call sites; the post-expansion artefact is the ground truth |
| 5-bench fat-LTO matrix | Hard gate §8 (clean across 5 benches) | The `cargo iter-check-full` source-level check already runs the same compilation; fat-LTO adds nothing the source verification doesn't catch except link-time errors — which are exceedingly rare on a 33k-line file that already passes `cargo check`. The bench RUN is what hard gate §8 means by "clean" — i.e. no panic on bench inputs. That can be done at `--profile profiling-prep` (thin LTO) at 1/3 the wall |
| samply × 4 | None at W0' specifically. W4.e demands samply at W4 close per W4.md:130-138 ("Expected hot path: BBNF parse path emits Seq/Alt/Rule compounds; FusedBuilder::push_leaf_*/begin_compound/end_compound appear ≥ 1% self-time"). W1.c demands per-fixture samply per W1.md:246-272. No W0' invariant reads samply content. | YES — every W0'-required claim about retired symbols is proved by `rg` at source (invariants §1, §2, §6, §15) plus `nm` at compiled artefact (invariant §3). samply only adds runtime self-time attribution which isn't claimed at W0'. |
| `nm` × 5 | Invariant §3 (note_push absent), §15 (push_compound symbol absent), W1/W4 nm gates (later waves) | YES — but `nm` on the existing bench binaries is cheap once binaries exist. The cost is the binary build, not nm itself. |

**Conclusion**: cycle-1 + 4 fresh expands is the load-bearing irreducible substrate verification. Everything else is either redundant with source-level grep or routes naturally to a later wave's hard-gate close.

## 4. Cycle-2 idempotency: load-bearing or theatre?

**Theatre at W0' close. Load-bearing at W4 close.**

The proc-macro cache at `crates/derive/src/lib.rs:128-175` is content-keyed on `(BBNF_SCHEMA_VERSION=16, CARGO_PKG_VERSION, all grammar file contents under @import-transitive closure, ParserAttributes, ident)`. Identical inputs → identical cache key → identical cached TokenStream. The cache write at line 213-224 uses atomic rename. The bootstrap script at `scripts/bootstrap-bbnf.sh:38-41` only clears `.bbnf-cache/` if `BBNF_BOOTSTRAP_CLEAN_CACHE=1` is set; the default is **no clear**, in deference to B1.W2.b's invariant 12 ("content-hash guard replaces the former unconditional rm").

Cycle-2 with cache PRESERVED is automatic determinism — it's a hash-table read, not a parse. The interesting determinism question is "given identical grammar inputs, does the 17-pass pipeline produce a deterministic TokenStream?" Cache-cleared cycle-2 tests this. **But:**

1. The 17-pass pipeline is deterministic by construction — it has no PRNG, no time-dependent ordering, no parallel-iteration ordering instability surfaced in any AY-I/AY-II audit. Determinism failure here would be a regression in a single pass, caught by the pass's own test suite long before bootstrap.

2. The post-expand Python regex pass at `scripts/bootstrap-bbnf.sh:47-345` is the more realistic non-determinism source — it does idempotency checks (`# Idempotent: if the line immediately before the struct is already our derive, don't re-add it.` at line 178) precisely because re-running over already-processed text is the more plausible drift surface. That's what `bash scripts/check-bootstrap-clean.sh` (W4.md:69) tests.

3. The HARDEST cycle-2 — the self-hosting identity check — is at W4 close per W4.md:1, 16, 56-66 ("Run `bash scripts/bootstrap-bbnf.sh`; capture as `/tmp/gen1.rs`. Clear caches; re-run; capture as `/tmp/gen2.rs`. `diff /tmp/gen1.rs /tmp/gen2.rs` must be empty"). At W0', the parser doing the parsing during regen is the OLD parser (from pre-W0' generated.rs); at W4, the parser doing the parsing has BEEN MODIFIED by W4's annotation completeness work, and self-hosting identity is the stronger property. **W4 is where cycle-2 actually proves something the source landings don't.**

At W0', cache-preserved cycle-2 is sufficient (proves the cache hit path returns identical bytes — trivially). Cache-cleared cycle-2 doubles the wall to verify a property already underwritten by the cache key's content addressability. **Defer cache-cleared cycle-2 to W4.**

## 5. W1 actual preconditions

Walking `waves/W1.md` for what it specifically demands from W0':

| W1 demand | W0' close requirement | Citation |
|---|---|---|
| W1 dispatches against post-W0' regen | YES — needs the freshly regenerated `generated.rs` reflecting W0'.a/b/c source landings (`Parsed::new_fused_output`, FusedBuilder symbols, projection consumer wiring) | W1.md:1-7 ("Opens after: W0' close ... bbnf_value_* rides the fused pipeline"); W1.md:108-109 ("Do NOT touch ... `crates/core/src/grammar/generated.rs` (regen is orchestrator-owned at W0' close)") |
| W1 requires samply baseline captures | NO — W1.c authors its own samply captures (5 fixtures into `.profiles/samply/AY-II-W1/json/<fx>/`) and reads its own data | W1.md:222-272 (W1.c §Samply per-fixture attribution) — entirely self-contained |
| W1 requires nm verification | NO at W0' — W1.c does its own nm runs into `post-AY-II-W1-nm-json.txt` | W1.md:240-251, hard gate §13 |
| W1 requires fat-LTO 5-bench at W0' close | NO — W1.b runs its own competitor + value bench captures; hard gate §16 ("`make ay-bench-close WAVE=W1-close` across all five bench binaries shows no JSON regression vs W0'-close and no panic on non-JSON grammars") references W0'-close as a CROSS-CHECK, not a precondition. If W0'-close benches don't exist, W1 can run a `WAVE=pre-W1` capture as its own anchor and proceed. | W1.md:380-385 |
| W1 requires `Parsed::to_value()` non-panic | YES — every W1 parity test (`sonic_rs_parity`, `value_api_apples_to_apples`, `json_admission_totality`) calls into `to_value()` | W1.md:5, 49, 274-326 |
| W1 requires projection_totality green | YES — W1.a's `json_admission_totality.rs` test depends on `JsonParser::PROJECTION_*` slices being coherent (already landed in projection_totality.rs:373) | W1.md:111-145 |

**W1 needs from W0' close**: a regenerated `generated.rs` that compiles, on which `Parsed::to_value()` is non-panicking (W0'.b wired this), and on which `projection_totality.rs` is green (already landed). Everything else W1 produces itself.

## 6. Compressed-honest W0' proposal

Replace W0p.md §Orchestrator-owned close ceremony with the following:

1. **Cycle-1 regen** (one wall payment): `bash scripts/bootstrap-bbnf.sh`. Capture wall-clock as the W0' baseline datum. Expected: 3–6 min on post-d3 substrate per `audit/W0p-regen-root-cause.md:181-187`.

2. **`cargo iter-check-full`** (compile parity gate): exits 0 on the post-regen `generated.rs`. Pays the proc-macro cache wall once, then warms it. Expected: < 2 min cold (per the d4–d7 + d3 baselines at `PROGRESS.md:340-348`).

3. **One fresh expand for primary verification**: `cargo expand -p bbnf --bench json_monolithic > target/expand/ay-ii-W0p-json.rs`. Verify by `rg`:
   - `materialize_projection_*_JsonParser` invocations present inside `project_value_JsonParser` (invariant §16).
   - `cursor.object_key_seek|bounded_lookahead|scan_structural_bounded` invocations present inside `__path_walk` (invariant §17).
   - Zero `parse_with_visitor_JsonParser` references.

   The other three grammars' fresh expands defer to their respective semantic-parity waves (W1.c for JSON additional, W2 for CSS, W3 for Sheets, W4 for BBNF) where the expand is the wave's own hard-gate evidence.

4. **`projection_totality` test green**: `cargo test -p bbnf --test projection_totality --profile ax-iter` exits 0. Already lands at master under d-sub-phase work; rerunning it on the post-regen substrate verifies invariant §7.

5. **`value_api_apples_to_apples` test green** under `--release`: proves `Parsed::to_value()` non-panic across the parity corpus (W0p.md hard gate §6, AY-II.md §Defensible floor 2).

6. **Source-level invariant grep** (substitutes for `nm`/samply at this wave):
   - `rg 'pub struct ValueBuilder|pub struct ValueBuilderOutput' crates/` → 0 (W0p.md hard gate §1).
   - `rg 'pub fn push_compound|pub fn mark_children' crates/tape/src/builder.rs` → 0 (hard gate §2).
   - `rg 'parse_with_visitor|navigate_tape|note_push' crates/core/src/runtime/parsed.rs` → 0 (AY-II.md invariants §1, §6, §3).

7. **PROGRESS.md W0' close entry** with the cycle-1 regen wall-clock + commit SHAs.

**Estimated total wall**: ~10–15 min on post-B1 substrate. (Cycle-1 regen 3–6 min + iter-check-full 2 min + one expand 2–3 min + tests 1–2 min + greps + bookkeeping seconds.)

**Deferred to later waves (with rationale)**:

- Cycle-2 idempotency → W4 close (the cycle-2 test there is the harder self-hosting identity, which is what `cycle-1 = cycle-2 byte-identical` actually means).
- Fresh expands × 4 → W1.c (JSON), W2 (CSS), W3 (Sheets), W4 (BBNF) — each wave has its own expand-as-evidence demand for its grammar.
- fat-LTO 5-bench matrix → W1 close (W1.b runs the JSON value bench at fat-LTO; W0p.md:255-260 already admits W0' uses `profiling-prep` not fat-LTO; if W0' doesn't need fat-LTO, the matrix is W1-and-later business).
- samply × 4 → W1.c (JSON × 5 fixtures), W2.c (CSS), W3.b (Sheets), W4.e (BBNF) — each wave's own hard gate demands the captures it needs.
- `nm` × 5 → W1.c, W4.e — same routing.

This compressed close inherits the AY-II thesis without theatre, pays the wall exactly once at W0' (the regen), and routes every measurement step to the wave whose hard gate demands it.

## 7. Forward dispatch order

A) **Compressed-honest W0' close** (per §6 above). Single dispatch, 10–15 min wall, lands on master with: post-W0' regen complete, projection_totality green, value_api_apples_to_apples green, fresh JSON expand captured as evidence, source-level invariant greps clean, PROGRESS.md updated.

B) **W1 dispatches next**, with 4 sub-agents per W1.md. W1.a (admission totality test), W1.b (competitor bench + value bench at fat-LTO — its own bench captures), W1.c (samply × 5 fixtures + nm), W1.d (parity-test tightening + parsecount wire-contract). W1's own hard gates self-contain the JSON-side measurement story.

C) **W2-W5 dispatch sequentially per existing wave specs.** No W0'-deferral creates a debt in W2/W3 — those waves' hard gates already demand their own grammar's expand + samply + bench + nm, independent of any W0'-close artefact stockpile. W4 close is where cycle-2 byte-identity is proven (self-hosting identity, the harder property cycle-2 actually tests). W5 is the cross-grammar matrix close.

**The W0p.md §Hard gate (wave close) list (items 1–10) maps cleanly onto the compressed proposal**: §1, §2 retained as source greps; §3, §4, §5, §6 verified via the JSON expand + projection_totality + value_api_apples_to_apples; §7 deferred to W4 with the cycle-2 caveat above; §8 (`make ay-bench-close WAVE=W0p-close`) downgraded to "iter-check-full clean + JSON value-bench smoke" with fat-LTO routed to W1; §9 retained as source-level grep; §10 retained as the projection_totality test. **Only §7 and §8 see substantive change**, and both have rationale that W4 / W1 hard gates respectively will redress.

## Dispatch advisory

The audit recommends the user adopt the compressed-honest W0' close (§6) and proceed directly to W1 dispatch on master post-close. The W0p.md text is not retired — its §Architectural thesis, §Invariants, §Scope, and §Hard gate remain authoritative; only §Orchestrator-owned close ceremony compresses to §6 above and §Hard gate §7/§8 carry the cycle-2 → W4 / fat-LTO → W1 deferral notes.

Wall savings: ~3.5 hours per W0' close on current hardware (compressed: 10–15 min vs as-written: ~4 hours). Honest stance: the savings come from deleting theatre, not from skipping verification. Every invariant W0' actually claims is still verified — just at the wave whose hard gate the verification serves.
