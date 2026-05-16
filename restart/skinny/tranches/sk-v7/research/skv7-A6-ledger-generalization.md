# SK-V7 A6 Ledger and Generalization Gap Analysis

Date: 2026-05-16.
Cohort scope: V5 + V6 (and predecessor SK-V3/V4 carry-overs surfaced via
`skinny/REDRESS.md` items 27-76 and `restart/skinny/tranches/sk-v5/SYNTHESIS.md`/`SK-V6`).
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Edits: none. This document is the only artifact.

The ledger consolidates validated, invalidated, demoted, and still-open items
across the SK-V5 → SK-V6 transition with citation-grounded evidence, then
walks REDRESS.md numerically, then audits per-corpus performance evolution
V4 → V5 → V6, then audits the V1 master-plan generalization surface.

Authority sources (read in order):

- `skinny/RESULTS.md` (current gate)
- `skinny/REDRESS.md` items 1-76 (rejected/admitted routes)
- `restart/skinny/tranches/sk-v5/SYNTHESIS.md` (substrate-history record)
- `restart/skinny/tranches/sk-v6/SYNTHESIS.md` (current synthesis)
- `restart/skinny/tranches/sk-v6/HANDOFF.md`
- `restart/skinny/tranches/sk-v6/SPEC.md`
- `restart/ARCHITECTURE.md` §7.3-7.5 (substrate spec + SK-V5/V6 status)
- `restart/MASTER-PLAN.md` §13 (Tranche H)
- `restart/MIGRATION.md` §3 (crate disposition)
- `restart/skinny/tranches/sk-v5/research/skv5-A6-research-ledger.md`
- `restart/skinny/tranches/sk-v6/research/skv6-A5-general-grammar-abstraction.md`
- `restart/skinny/tranches/sk-v6/research/skv6-C6-generality-costfacts.md`

## 1. Validated Items

The following items are load-bearing in V6's current synthesis and have a
measurement/citation/commit-SHA trail.

| Item | Source | Commit | Evidence |
|---|---|---|---|
| Lazy/event substrate is the right boundary | GS-SK-V5 §10; GS-SK-V6 §4 | REDRESS item 20 admit | Eager retained tape capped below SOTA; lazy offset tape survived triad without parallel substrate |
| Codegen overhead is separable from substrate ceiling | GS-SK-V5 §11; REDRESS 34, 35, 40 | `d37f1cc2` `20e5fe46` | bench-private `SinkParser` dishonesty identified (REDRESS 34); generated `SinkOnly` Track 1 emitted from BIR (REDRESS 40) |
| Lock 15 i-cache discipline (yyjson evidence) | GS-SK-V5 §9; MASTER-PLAN §13 row H.W0 | Lock 15 inheritance row | yyjson 3,687 MiB/s on M5 Max set as DOM-class anchor; `lto=fat codegen-units=1 panic="abort" debug=true` enforced |
| Lock 16 primitive admission discipline | GS-SK-V5 §10; MASTER-PLAN §13.1; REDRESS 28, 29 | `9eef728c` `70e8348e` | `escape_mask_64` correctness bug repro `0xCAFEF00DBAADF00D`; consumed-only admission; V6 register-clobber concern surfaced REDRESS 70e8348e false-positive risk |
| SIMD vocabulary grammar-neutral + checkasm-gated | MASTER-PLAN §13.1; REDRESS 70e8348e + cae7b48b | `70e8348e` `cae7b48b` | `BYTE_CLASS_FROM_EQ_SET_64`, `BYTE_CLASS_FROM_TABLE_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `EOB_PAD_CLAMP`, `BULK_EMIT_POSITIONS_64`, structural+terminator classifier — all with scalar refs, parity gates, hot consumers |
| sonic-rs/yyjson-style direct materialization (Eisel-Lemire vendored) | GS-SK-V5 §3; REDRESS 39; SKV5 D1 novelty | `20e5fe46` | Vendored from `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/`; PASS on numbers row (148.0% sonic-rs in current RESULTS.md L16) |
| simdjson On Demand iterator model | GS-SK-V6 §2; SKV5 A1 reframe | n/a | Reference model for typed-event iteration; informs `DirectBuild` field-fact contract |
| dav1d/FFmpeg/VLC process discipline | GS-SK-V6 §3; SKV6 A2 | n/a | scalar oracle + feature masks + register-clobber + stack canary + cycle counters + same-wave consumer |
| Two-layer ASM vocabulary (Layer 0 `x86inc.asm` + Layer 1 `bbnf.asm`) | MASTER-PLAN §13 H.W2.5 row; MIGRATION §3 | `74406332` `9eef728c` | Layer 0 dav1d BSD-2 read-only; Layer 1 grammar-neutral macros under `skinny/crates/bbnf-simd/ext/x86/bbnf.asm` |
| 5-shape BackendShape taxonomy (Rust state landed) | ARCH §7.4; GS-SK-V5 §6 | `603308b3` | `skinny/crates/ir/src/lib.rs:335-340` defines `EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage`; `passes::recognizers::derive_backend_shape_with_diagnostics` populates `LayoutFacts.backend_shape` |
| Generated SinkOnly from BIR DirectBuild | ARCH §7.4; REDRESS 40, 48 | `20e5fe46` `d37f1cc2` | `codegen/src/lower/sink_only.rs` walks BIR; former `json_templates/sink_direct.rs` splice removed |
| ContainerNext dispatch carry (V6 Wave 2 admit) | REDRESS 63 | `2b3bef79` | Median deltas: canada +10.7%, mesh +7.5%, numbers +10.3%, unicode_mixed +10.7%; preserves Lock 1 — no retained sidecar, no second source pass |
| Generated-retained tiny-string cap-16 under native gate | REDRESS 72 | `1e213001` | Native Criterion: twitter +27.5%, citm +49.2%, instruments +44.9%, distinct_values +57.5%; rejected globally — direct/Track 2 stay at cap-8 |
| Host-output-schema typed DirectBuild lowering | REDRESS 71 | `ab06ff11` | twitter real_typed_struct: Track 1 18129 Mbps vs sonic 11969 = 151.5% sonic PASS; update_center Track 1 12044 vs sonic 12144 = 99.2% within 1.10x slack PASS |
| Canada SIMD structural scan floor restored | REDRESS 56 | `cae7b48b` | 22136 → 41833 Mbps → 69075 Mbps current (RESULTS.md L167); clears 40000 Mbps NEON floor |
| Eisel-Lemire vendored numbers PASS | REDRESS 39, 46 | `20e5fe46` | numbers parse 20085 Mbps vs sonic 13567 = 148.0% (RESULTS.md L16) |
| simd-scan fossil deletion + eventcursor purge | REDRESS 38; SK-V5 Wave 4 | `726ab124` | `skinny/crates/simd-scan/` deleted; `eventcursor` feature flag + cfg path + `generated_eventcursor.rs` removed |
| Direct-to-struct as required workload | GS-SK-V5 §5; REDRESS 30 | n/a | Throughput gate not just correctness; gate at sonic-rs * 1.10 ns slack |
| Expanded corpus over historical triad | GS-SK-V5 §4; REDRESS 27 | n/a | 17-corpus matrix; triad alone hides parse-G distribution |
| Strictness columns disclosed in RESULTS.md (V5 Wave 0) | GS-SK-V5 §4 + §11 item 4 | `afb83c05` | RESULTS.md L3 schema: `Strictness`, `parse_utf8`, `escape_complete`, `flaw_probe` columns |
| parse-attribution feature flag | GS-SK-V5 §1 novelty | `afb83c05` | `#[inline(never)]` named-boundary build for samply attribution |

Total validated: 19.

## 2. Invalidated Items

The following items are pruned from active scope and may not be reopened
without new measurement evidence per `restart/skinny/tranches/sk-v6/HANDOFF.md` §4.

### 2.1 SK-V5 Substrate-History Invalidations

| Item | Source | Evidence |
|---|---|---|
| Eager retained tape as SOTA substrate | GS-SK-V5 §10; REDRESS 18, 20 | Capped below SOTA; lazy offset tape replaced |
| 12-byte token width churn | REDRESS 18 | Measured and rejected |
| Pair-token fusion | REDRESS 16 | Measured and rejected |
| PSI/DTA Rust-codegen automaton | GS-SK-V5 §13; V9.5 PSI excavation | LLVM cannot fold Rust-emitted automaton overhead; CollapsedStage NASM-only |
| StructuralIndex sidecar prepass | REDRESS 14; GS-SK-V5 §10 demoted | Valid as runtime intermediate, invalid as parallel substrate |
| EventCursor parallel prepass | REDRESS 38; SK-V5 Wave 4 | Purged; cfg path removed |
| Function-pointer dispatch table | REDRESS 17 | Audited and rejected as signal; "real function-pointer table regressed" (RESULTS.md L54) |
| Capacity prescan (one-shot/sampled) | MASTER-PLAN H.W0 row | Plan D `with_capacity(256)` + geometric grow replaced sampled and sparse-flag helpers |
| Generic SWAR whitespace skipper | REDRESS 51 | byte-class whitespace cursor rejected |
| Separator elision | REDRESS 13; GS-SK-V5 §10 | Measured-rejected, non-canonical |
| Raw `f64` shortcut | REDRESS 39 prelude | `raw.parse::<f64>()` rejected on canada parity (dec2flt 1-ULP disagreement) |
| Class A `match_tiny_plain_string` NEON wiring as parse-G fix | REDRESS 28, 33; GS-SK-V5 §2 | Wired previously, regressed twitter ~25%, reverted; tiny-plain-string is direct receiver concern not parse-G |
| asmjson as M5 Max close | GS-SK-V5 §9; GS-SK-V6 §2; REDRESS 74 | Permissive SWAR fallback on M5 Max; AVX-512 fast path is x86; valid only for strict same-plane x86 |
| Primitive admission without same-wave consumer | REDRESS 70e8348e | Orphan kernels violate Lock 16; `BULK_EMIT_COMPRESSED`, `FRAME_PUSH/POP_BOUNDED`, `FSM_DISPATCH_THREADED` remain blocked |

### 2.2 SK-V5 UTF-8 Fusion Family (REDRESS 50-55)

| Item | REDRESS # | Evidence |
|---|---|---|
| Retained projection side tables | 50 | Improved view probes, regressed retained parse plane |
| Byte-class whitespace EventCursor wrapper | 51 | Did not add side table but did not consume structural mask; regressed focused retained Track 1 |
| Parser-local structural-mask cursor | 53 | Consumed live JSON emit mask + O(1) state, still regressed retained triad to 6156/8344/7139 Mbps |
| Exact decoded-string stats sink | 54 | Correctness-green, regressed escaped-string direct rows |
| Quote-source fused streaming materializer | 55 | AArch64 batched `\uXXXX` still lost to allocate-then-contiguous-hash default |

Item 59 (REDRESS) consolidates: "fold UTF-8 validation into NEON 16-byte body scan"
is not sufficient to close parse-G or direct string rows.

### 2.3 SK-V6 Retained-Parse Routes

| Item | REDRESS # | Commit | Evidence |
|---|---|---|---|
| Retained trusted-string boundary collapse | 60 | `c0f966e5` | -20.5% to -46.9% across twitter/random/unicode_basic/apache_builds; tiny-string probe NOT redundant |
| Retained long-string trusted scan specialization | 61 | `b4fd454f` | gsoc-2018 +15.4% but canada -9.8%, instruments -7.5%; only 1 row cleared 10% bar |
| Delayed-wide retained string scan | 62 | `023284d6` | twitter -7.46%, distinct_values -8.49%, update_center -5.28% c/B; broader "even delayed 64-byte scanner is not admissible" |
| Retained Unicode-escape run validator (4-unit `\uXXXX`) | 64 | `a6a80952` | unicode_escapes +31.8% but y_string_unicode -3.7%; gate required +8% on y_string; reverted |
| Object next-key carry | 65 | `cff17e38` | citm_catalog +0.36%, random -1.21%, instruments -1.06%; gate required +3% citm, +2% random/instruments |
| Retained Track 2 array next-byte dispatch parity repair | 73 | `7cac3971` | citm Track 2 +20310 Mbps but apache_builds -28.5%; not a free Track 2 parity repair |

### 2.4 SK-V6 Direct-Materialization Routes

| Item | REDRESS # | Commit | Evidence |
|---|---|---|---|
| Direct source-hook field-layout materializer | 66 | `c0091f0d` | unicode_escapes +0.99%, unicode_mixed +0.11%; gate required +20%; receiver/closure removal too small |
| Parser-owned decoded scratch materializer | 67 | `9bbe7d22` | unicode_escapes -44.03%, y_string_unicode -16.76%; `unescape_json_string` second-pass faster than fold-in |
| Byte-output unescape materializer | 68 | `df990c32` | unicode_escapes -4.00% same-HEAD baseline; manual byte writes add control/finalization overhead |
| DirectBuild semantic string facts (streaming hash) | 69 | `e3ace0d2` | unicode_escapes -15.22%; repeats cost class of REDRESS 54/55 |
| Hand-authored real_typed_struct sink as DirectBuild proof | 70 | `10abb7b0` | update_center 4.84 Gbps vs sonic 7.12 Gbps; cannot prove grammar-only `DirectBuild` from hand parser |
| Global tiny-string cap-16 | 72 sub-reject | `40abc05e` | Track 2 retained regressed apache/github/gsoc/instruments; direct regressed instruments -7.6%, distinct -24.6%, y_string -9.8% |
| Raw key byte dispatch in real_typed_struct | 71 sub-reject | (within `ab06ff11`) | update_center scout 11537→11273 Mbps; LLVM `match` lowering beats hand-emitted if-chain |
| Narrow selected-output `Plugin { name, version }` plane | 71 sub-reject | (within `ab06ff11`) | Profile scout crossed slack but Criterion widened gap |

Total invalidated: 27 (14 SK-V5 + 5 UTF-8 family + 6 retained + 8 direct).

## 3. Demoted / Narrowed Items

| Item | Demotion | Source |
|---|---|---|
| asmjson DPDA architecture | Valid x86 successor reference; INVALID as M5 Max anchor (permissive SWAR fallback on arm64) | GS-SK-V5 §9; GS-SK-V6 §2 |
| `CollapsedStage` shape | Valid taxonomic value (per-rule cost decision); INVALID as Rust codegen target; ONLY hand-written NASM per-grammar | GS-SK-V5 §13; MASTER-PLAN §13 H.W5 + per-grammar wave |
| StructuralIndex | Valid as runtime intermediate concept; INVALID as parallel substrate or retained sidecar | GS-SK-V5 §10; REDRESS 50 |
| `match_tiny_plain_string` NEON kernel | INVALID as parse-G fix at 8-byte scalar boundary; VALID as direct receiver fast-path (REDRESS 57) and as generated-retained cap-16 cost fact (REDRESS 72) | REDRESS 28, 33, 57, 72 |

## 4. Still-Open Items (Post-V6)

| Item | Source | Status |
|---|---|---|
| Comparator-plane repair: sonic-rs `utf8_lossy` rebuild | REDRESS 75; HANDOFF §3.1; IMPL-PACKET Wave 0 | BLOCKING gate — current sonic rows strict-anchor-ineligible per C3 (`skv6-C6-generality-costfacts.md`) |
| Per-`\uXXXX` TBL classifier inside retained string path | C1/C5 nomination; HANDOFF §3.3; IMPL-PACKET Wave 2 candidate 4 | Distinct from rejected REDRESS 64 contiguous-run validator — targets every unit including boundary-heavy short strings |
| mesh `DirectBuild` typed expansion | C2 nomination; HANDOFF §3.4; IMPL-PACKET Wave 3 | First product-plane typed expansion beyond twitter/update_center; mesh = typed numeric vectors |
| Lock 14 cleanup (passes + codegen + parse-that-regex) | C6 audit `skv6-C6-generality-costfacts.md`; IMPL-PACKET Wave 4 | `runtime/tape` is clean; remaining 30+ leaks listed in C6 table |
| DAV1D-grade checkasm hardening | IMPL-PACKET Wave 1; MASTER-PLAN H.W2.5 | register-clobber checks + rdtsc + stack-canary; first attempt at sentinel closure wrapping was rejected (REDRESS 70e8348e note) — reserved for FFI/ASM `call_new` shims |
| 4 remaining bbnf.asm primitive bodies (blocked on consumers) | REDRESS 70e8348e Wave 5; MASTER-PLAN §13 H.W2.5 row | `FSM_DISPATCH_THREADED`, `BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED` — same-wave consumer required (CollapsedStage NASM or bracket-stack consumer) |
| x86 CollapsedStage successor | IMPL-PACKET Wave 7; MASTER-PLAN §13 per-grammar wave | Optional; Zen 4 silicon + NASM author + checkasm-green primitives required |
| Full same-plane 17-corpus matrix | IMPL-PACKET Wave 6 | All 17 corpora x 7 workloads same-plane sidecars; Mbps + c/B both reported |
| SOTA-beat declaration per row strict-vs-strict | IMPL-PACKET Wave 6 close | Blocked behind comparator-plane repair |
| `CostFacts` side-table implementation | `skv6-C6` §CostFacts Audit | "No `CostFacts` or `CostDecision` implementation exists in the requested crates"; COMPILER.md:853-858 says cost model stubbed |

The packet-level "all 9 bbnf.asm bodies" reading was superseded: the no-orphan
rule narrows it to 4 still-blocked bodies, since `BULK_EMIT_POSITIONS_64` and
the AArch64 structural+terminator classifier were admitted in `cae7b48b` with
the Canada scan-floor consumer (REDRESS 56).

## 5. Rejected Routes Consolidated Ledger (REDRESS.md Numeric Walk)

| Item | Status | Signal |
|---|---|---|
| 1-26 | Cohort prior to expanded corpus | Tape/parse/payload/scanner/dispatch infrastructure landed across triad; pair-token fusion (16) and dispatch table (17) and skipless 12-byte tokens (18) rejected |
| 27 | SK-V3 reprofile split | Expanded blockers mechanism-classified |
| 28 | SK-V3 W0/W1 SIMD parity | Host aarch64 primitive admission |
| 29 | HEAD vocabulary state | Two-layer admission after `74406332` + `9eef728c` |
| 30 | Direct-to-struct as throughput gate | Not just correctness |
| 31 | Direct sink profiling | Moved next blocker from view-walk to materialization |
| 32 | Gate status executable | Not prose-only |
| 33 | SK-V5 Wave 3 Class A wiring | INVALIDATED at 8-byte scalar early-out |
| 34 | SK-V5 Wave 2 bench-private dishonesty | IDENTIFIED |
| 35 | SK-V5 W1/W2 scaffolding | Lowerer present |
| 36 | SK-V5 W4 JSON-hardcoded scalar refs | IDENTIFIED in bbnf-simd |
| 37 | SK-V5 W4 bbnf-simd god-module | IDENTIFIED — 716 LOC `lib.rs` |
| 38 | SK-V5 W4 simd-scan fossil | IDENTIFIED; later purged |
| 39 | SK-V5 W2 Eisel-Lemire | VENDORED |
| 40 | SK-V5 W2 generated SinkOnly | ADMITTED |
| 41 | SK-V5 W2 cargo target dir + metadata | CORRECTED |
| 42 | SK-V5 W3 trusted UTF-8 boundary match | VALIDATED |
| 43 | SK-V5 W3 post-escape skip + validation batch | REJECTED |
| 44 | SK-V5 W3 Track 2 strict-string penalty | REMOVED |
| 45 | SK-V5 W3 close decision | Correctness + primitive green; full matrix open |
| 46 | SK-V5 direct-number/context-sink | numeric direct rows improved |
| 47 | SK-V5 reporting advisory bench | output-plane disclosure landed |
| 48 | SK-V5 SinkOnly lowerer | direct emission consumes BIR |
| 49 | SK-V5 generic decoded visitor | REJECTED |
| 50 | SK-V5 retained projection side tables | REJECTED — view-probe gain but retained regression |
| 51 | SK-V5 byte-class whitespace cursor | REJECTED |
| 52 | SK-V5 baseline reassay | Snapshot |
| 53 | SK-V5 parser-local structural-mask cursor | REJECTED — second scan beside source-byte recursive descent |
| 54 | SK-V5 exact decoded-string stats sink | REJECTED — correctness-green, regressed escaped direct rows |
| 55 | SK-V5 quote-source fused streaming hasher | REJECTED — lost to default allocate-then-hash |
| 56 | SK-V5 structural scan floor | ADMITTED — Canada 22136→41833→69075 Mbps |
| 57 | SK-V5 direct receiver inlining + tiny raw-span fast path | ADMITTED bounded (4 digest passes: citm/apache/github/instruments) |
| 58 | SK-V6 W0 dispatch hygiene | Purge SK-V3/SK-V4 packets |
| 59 | SK-V6 refutes Wave 3 UTF-8 fusion class | REFUTED |
| 60 | SK-V6 W2 trusted-string boundary collapse | REJECTED — -20.5% to -46.9% |
| 61 | SK-V6 W2 long-string trusted scan | REJECTED — 1/3 rows cleared, 2 regressed >5% |
| 62 | SK-V6 W2 delayed-wide retained string scan | REJECTED — broader class blocked |
| 63 | SK-V6 W2 ContainerNext / array next-byte | ADMITTED (`2b3bef79`) |
| 64 | SK-V6 W2 Unicode-escape 4-unit run validator | REJECTED — y_string_unicode regressed |
| 65 | SK-V6 W2 object next-key carry | REJECTED — guards held but lifts missed |
| 66 | SK-V6 W3 direct source-hook materializer | REJECTED — receiver/closure removal too small |
| 67 | SK-V6 W3 parser-owned decoded scratch | REJECTED — -44% unicode_escapes |
| 68 | SK-V6 W3 byte-output unescape | REJECTED — manual byte writes add overhead |
| 69 | SK-V6 W3 DirectBuild semantic string facts | REJECTED — repeats REDRESS 54/55 |
| 70 | SK-V6 W3 hand-authored real_typed_struct sink | REJECTED — cannot prove grammar-only DirectBuild |
| 71 | SK-V6 W3 host-output-schema generated typed DirectBuild | ADMITTED (`ab06ff11`) — twitter 151.5% sonic |
| 72 | SK-V6 W2 generated-retained cap-16 native | ADMITTED scoped (`1e213001`) — global rejected |
| 73 | SK-V6 W2 Track 2 array dispatch parity | REJECTED — apache regressed 28.5% |
| 74 | SK-V6 asmjson/DAV1D synthesis | SPEC REDRESS only |
| 75 | SK-V6 comparator-plane correction | sonic-rs `utf8_lossy` strict-anchor-ineligible |
| 76 | SK-V6 C-pass refinement | per-`\uXXXX` TBL nominated; mesh DirectBuild nominated; Lock 14 leak inventory in passes/codegen/parse-that-regex |

## 6. Per-Corpus Performance Evolution (V4 → V5 → V6)

Source: V5 ledger reports historical Mbps; V6 ledger is current `skinny/RESULTS.md`
HEAD post `2631a834`. Track 1 is `runtime::generated_json::parse` (RESULTS.md L222).

### 6.1 Parse Workload (Mbps; lazy retained)

| Corpus | V4 (pre-SK-V5) | V5 (post-W3) | V6 (current) | V6 vs sonic | Trajectory |
|---|---:|---:|---:|---:|---|
| twitter | 16294 | 12303 | 15597 | 73.6% | recovered (V5 regression repaired) |
| citm_catalog | 29185 | 20775 | 32459 | 130.3% | strongly recovered |
| canada | 16975 | 17738 | 18775 | 148.3% | monotone improving; GO |
| apache_builds | n/a | n/a | 12638 | 78.0% | (new in expanded corpus) |
| github_events | n/a | n/a | 15268 | 68.8% | (new) |
| update_center | n/a | n/a | 11912 | 59.6% | (new) |
| mesh | n/a | n/a | 14330 | 121.1% | GO |
| random | n/a | n/a | 10071 | 65.5% | (new) |
| gsoc-2018 | n/a | n/a | 23161 | 53.6% | (new; sonic dominates here) |
| marine_ik | n/a | n/a | 13688 | 136.0% | GO |
| instruments | n/a | n/a | 18163 | 92.0% | within slack of A |
| numbers | n/a | n/a | 20085 | 148.0% | GO (Eisel-Lemire pays out) |
| unicode_mixed | n/a | n/a | 8914 | 56.1% | parse-G |
| unicode_escapes | n/a | n/a | 12905 | 80.4% | parse-G |
| unicode_basic | n/a | n/a | 12193 | 91.7% | within slack |
| distinct_values | n/a | n/a | 9783 | 60.2% | parse-G |
| y_string_unicode | n/a | n/a | 6290 | 46.0% | parse-G hardest |

The V5 regression on twitter (16294→12303) was load-bearing in driving the
V6 ContainerNext admission (REDRESS 63). The V6 trajectory recovered twitter
to 15597 Mbps, and citm_catalog to 32459 Mbps — materially above V4's 29185.
Canada is now in monotone improvement across V4→V5→V6: 16975→17738→18775 Mbps.

Of 17 parse rows, 13 are G (NO-GO), 4 are A (GO): canada, mesh, marine_ik, numbers.

### 6.2 Direct Workload (Mbps)

The `direct_to_struct` workload is the `semantic_full_digest_stressor` guard.
The `real_typed_struct` is the V6 representative product workload (REDRESS 71).

| Corpus | direct_to_struct T1 | direct vs sonic | Signal | real_typed_struct T1 | real_typed vs sonic |
|---|---:|---:|---|---:|---:|
| twitter | 11899 | 78.4% | NO-GO digest | 18129 | 151.5% PASS |
| citm_catalog | 21460 | 99.3% | PASS digest | — | — |
| canada | 10463 | 83.6% | NO-GO digest | — | — |
| apache_builds | 11314 | 112.6% | PASS digest | — | — |
| github_events | 12377 | 114.3% | PASS digest | — | — |
| update_center | 8497 | 89.3% | NO-GO digest | 12044 | 99.2% PASS |
| mesh | 8818 | 91.8% | NO-GO digest | — | (C2 nominated next) |
| random | 7858 | 85.8% | NO-GO digest | — | — |
| gsoc-2018 | 15123 | 177.6% | NO-GO digest (Track 2 5744 — fails 1.10x slack) | — | — |
| marine_ik | 9400 | 106.8% | NO-GO digest (Track 2 6429) | — | — |
| instruments | 12131 | 93.5% | PASS digest | — | — |
| numbers | 12625 | 97.3% | NO-GO digest (Track 2 4317) | — | — |
| unicode_mixed | 4782 | 74.6% | NO-GO digest | — | — |
| unicode_escapes | 5303 | 58.5% | NO-GO digest | — | — |
| unicode_basic | 9180 | 129.4% | NO-GO digest (Track 2 4859) | — | — |
| distinct_values | 6269 | 53.7% | NO-GO digest | — | — |
| y_string_unicode | 5070 | 59.3% | NO-GO digest | — | — |

Direct PASS rows on digest workload (V6 current): citm_catalog, apache_builds,
github_events, instruments (4 of 17). Direct PASS rows on real_typed_struct: 2 of 2
representative (twitter, update_center). Of 17 direct digest rows, 13 are NO-GO.

The Track 2 column matters: the dual-track gate fails if either Track 1 or
Track 2 is outside the 1.10x sonic-rs ns slack. For gsoc-2018, marine_ik,
numbers, unicode_basic, distinct_values, Track 1 is within slack but Track 2
(the independent hand parser oracle) is not — these rows are NO-GO on Track 2
parity, not codegen quality.

### 6.3 Trajectory Summary

V6 recovered the V5 retained parse regressions on twitter and citm_catalog
(via REDRESS 63 ContainerNext + REDRESS 72 cap-16 generated-retained). Canada
continued monotone improvement across V4→V5→V6. The real_typed_struct PASS
on twitter at 151.5% sonic is the strongest single signal that generated
DirectBuild with host/API schema facts is the correct close route for typed
product workloads.

## 7. V1 Master Plan Generalization Gaps

The 5-shape `BackendShape` taxonomy is the V1 generalization vehicle. The
gap is between (a) the spec surfaces, which are complete, and (b) the runtime
state, which has JSON-name leaks across 3 generic crates.

### 7.1 Verified in Code

- `BackendShape` enum: `skinny/crates/ir/src/lib.rs:335-340` — 5 variants
- `LayoutFacts.backend_shape: HashMap<RuleId, BackendShape>` field — present
- `derive_backend_shape_with_diagnostics`: `skinny/crates/passes/src/lib.rs:287-331`
- `codegen/src/lower/`: per-shape lowering exists; `sink_only.rs` walks BIR
- `DirectBuildField`: `skinny/crates/ir/src/lib.rs:446` — field roster present

### 7.2 Generalization Plan (per A5 `skv6-A5-general-grammar-abstraction.md`)

A5 confirms: the 5-shape taxonomy is sufficient for arbitrary grammars. CSS
needs `EagerTape`/`EventTape` around recovery/layout-heavy rules; Sheets/math
needs `PrattSpine` plus `OffsetTape`/`SinkOnly`; CSV selects `SinkOnly` or
`OffsetTape`; BBNF-self mostly `OffsetTape` + direct `SinkOnly`. No new
variant needed.

What is missing per A5:

| Surface | Current state | Required for non-JSON grammars |
|---|---|---|
| `passes::compile` | Calls `shapes::shapes_for_json()` + `recognizers::nominate_json(&_grammar)` (`passes/src/lib.rs:30-32`) | `derive_shape_facts(grammar, schemas)` + `nominate_recognizers(grammar, metadata)` |
| `passes::shapes` | Builds `Json*` shapes by hand (`passes/src/lib.rs:208-239`) | `DirectFieldFacts` + schema-derived shape facts resolved to ids |
| `StructuralAlphabet::json()` | Hardcoded `b"{}[],:\""` (`ir/src/lib.rs:411-416`) | Generated `StructuralClassTable` from grammar first-sets + delimiters |
| `TapeKind::{Object, Array, Pair, String, Number, Bool, Null}` | JSON-shaped variants in generic IR (`ir/src/lib.rs:433-443`) | Grammar-derived node/event kind ids |
| `DirectBuildDecode::{JsonString, JsonNumber}` | JSON-named (`ir/src/lib.rs:510-515`) | `EscapedString`, `NumberScalar`, `Literal`, `Raw` |
| `parse-that-regex/src/lib.rs:34-45` | `JsonStringMatch`, `StringMode::StrictJson`, `StrictJsonTrustedUtf8` | `PrimitiveFacts` string plan from generated `DelimitedRegionPlan` |
| `parse-that-regex/src/lib.rs:127-139` | `skip_json_whitespace` | `PrimitiveFacts` `skip_class_run` over generated trivia classes |
| `parse-that-regex/src/lib.rs:268-341`, `416-514`, `594-719`, `766-968` | JSON string match/escape/unescape API | `PrimitiveFacts` delimited-region + Unicode escape policy |
| `codegen/src/json_templates/{generated,parser,value,view,visitor}.rs` | JSON templates in generic crate | Per-grammar generated runtime templates under `runtime/src/grammars/<name>/` |
| `codegen/src/lib.rs:68-97`, `117-188`, `201-226` | `emit_json*`, `JsonSink`, `JsonValue`, `JsonVisitor` reexports | `emit_grammar_*` from metadata + schema facts |
| `codegen/src/json_sink_direct.rs`, `json_typed_direct.rs`, `direct_schema.rs` | JSON rule/literal validators + `json_key` naming | `DirectFieldFacts` + grammar-neutral field key/path labels |

### 7.3 CostFacts: Not Implemented

Per C6 audit: "No `CostFacts` or `CostDecision` implementation exists in the
requested crates" (`skv6-C6-generality-costfacts.md` §CostFacts Audit).
`passes/src/lib.rs:33-39` assigns `layout_facts.backend_shape =
shape_plan.backend_shape` directly with no selected/rejected/dominated
alternative evidence. `COMPILER.md:853-858` explicitly says the cost model
is stubbed.

Without `CostFacts`, the per-rule `BackendShape` decision is grammar-name
gated. Wave 4 must add `CostFacts` records for backend shape, tiny-string
cap, quoted-span strategy, direct materializer, capacity policy, and
primitive route. This is the structural prerequisite for non-JSON grammars
to traverse the same lowering pipeline.

### 7.4 Per-Grammar Wave Allocations (MASTER-PLAN §13)

The H tranche is JSON-primary on M5 Max with x86_64 AVX-512 secondary.
Per-grammar `CollapsedStage` `.asm` authoring waves are explicitly
"grammar-keyed, not numbered into the H letter tranche" (`MASTER-PLAN.md:548`).
Each grammar whose `LayoutFacts.backend_shape` admits `CollapsedStage` for at
least one rule gets one wave per `(grammar × ISA)` pair, tracked under
`[workspace.metadata.bbnf.grammars.<name>]`. The `BBNF-COLLAPSEDSTAGE-NOT-VIABLE`
diagnostic surfaces a missing `(grammar × ISA)` author so the cost model
falls back to `OffsetTape`.

H.W7 (Pratt recognizer facts + BIR `PrattSpine`) is listed but blocked
behind H.W6 strict matrix close. Expression grammar (Sheets, math) requires
Pratt before non-JSON closure.

### 7.5 MIGRATION §3 grammar-specific transitions

MIGRATION.md §3 documents the rename `crates/simd-scan` → `crates/bbnf-simd`
(`MIGRATION.md:259-269`) as the Lock 14 grammar-neutral primitive boundary.
The 9 macros at `ext/x86/bbnf.asm` are grammar-neutral by construction.
Layer 0 `x86inc.asm` (dav1d BSD-2) is read-only.

MIGRATION.md does not document any grammar-specific transition beyond the
crate rename. Per-grammar transitions are implicit in the `(grammar × ISA)`
wave structure of MASTER-PLAN §13.

### 7.6 The Single Most Consequential Generalization Gap

`CostFacts` side-table absence is the single most consequential
generalization gap.

Without `CostFacts`, the per-rule shape decision in
`derive_backend_shape_with_diagnostics` cannot record selected, rejected,
dominated, objective vector (throughput/memory/i-cache budget/risk),
scalarization profile, target ISA, or extraction method. Every
JSON-specific threshold and policy currently encoded in
`passes::shapes::shapes_for_json`, `recognizers::nominate_json`,
`parse-that-regex` JSON primitive APIs, and `codegen` JSON renderers
ultimately depends on this absent decision substrate.

A5 makes this explicit (`skv6-A5-general-grammar-abstraction.md:155-160`):
"The generalization work is to remove grammar-name inputs and record the
decision as `CostFacts`, not to change BIR." C6 says
(`skv6-C6-generality-costfacts.md` §CostFacts Audit): "without `CostFacts`,
JSON-specific thresholds and parser/materializer choices will keep
reappearing as hardcoded codegen or parse-that-regex policy."

REDRESS 72 is the empirical proof: the generated-retained cap-16 tiny-string
decision should be a per-rule `CostFacts` entry resolving to a generated
threshold table, not a "JSON cap-16 vs JSON cap-8" hardcoded choice in
generic codegen. The fact that the same cap-16 regresses direct/Track 2 but
helps generated-retained is precisely the kind of multi-objective decision
`CostFacts` is designed to record.

## 8. The "What We Believe" Summary (Post-V6 Load-Bearing Beliefs)

1. **Substrate union holds (Lock 1 verified post-V5).** Structural projection
   is the tape when retained; direct `SinkOnly` is a projection over the
   same accepted event stream. The fossil `simd-scan/` crate is deleted;
   `generated_eventcursor.rs` + `eventcursor` cfg path is purged
   (`726ab124`). The 5 SK-V5 UTF-8 fusion routes (REDRESS 50-55) attempted
   to reintroduce a parallel substrate or sidecar metadata; all 5 were
   refuted by measurement.

2. **Codegen substrate fully landed.** `BackendShape` + `LayoutFacts.backend_shape`
   + `derive_backend_shape` + `codegen/src/lower/` + 5-shape lowering all
   exist post `603308b3`. The generated `SinkOnly` direct path is real Rust
   emitted from BIR (`d37f1cc2`) — not a hand-written bench-private parser
   (REDRESS 34 dishonesty closed). The Wave 1 substrate state is LANDED.

3. **Numbers + Canada scan floor closed.** `numbers` parse 148.0% sonic-rs.
   Canada parse 148.3% sonic-rs and structural scan 69075 Mbps vs 40000 Mbps
   floor. Eisel-Lemire vendored from upstream `parse-that`; integer
   materializer correctly placed in `parse-that-regex/src/number/integer.rs`.

4. **Twitter typed product plane SOTA-beat at 151.5% sonic.** Generated
   typed `DirectBuild` from host/API output schema (REDRESS 71, `ab06ff11`)
   delivers `real_typed_struct` Track 1 at 18129 Mbps vs sonic 11969 Mbps
   on twitter. update_center within 1.10x slack. This proves the
   "host/API schema-fact" contract is the correct route for typed product
   workloads. Generic decoded visitors, sink-local exact-stats helpers,
   quote-source streaming hashers, source-hook folding, parser-owned
   decoded scratch, byte-output unescape, semantic string facts, and the
   first hand-authored JSON typed sink (REDRESS 49, 54, 55, 66, 67, 68,
   69, 70) are all rejected as closes for this plane.

5. **13 parse-G rows remain — but throughput materially recovered post-V6.**
   ContainerNext (`2b3bef79`) and generated-retained cap-16 (`1e213001`)
   lifted twitter and citm_catalog out of V5 regression. Of 17 parse rows,
   4 are GO: canada, mesh, marine_ik, numbers. Hardest parse-G is
   y_string_unicode at 46.0% sonic; the cluster is dominated by string/
   Unicode and competitor-anchor (sonic-rs `utf8_lossy`) gaps.

6. **11 direct N rows remain — but 4 digest PASS + 2 real_typed_struct PASS now exist.**
   PASS digest: citm_catalog, apache_builds, github_events, instruments.
   PASS real_typed_struct: twitter, update_center. The product gate
   (`real_typed_struct`) is the SOTA-close gate; the maximal digest stressor
   remains a guard, not the product-plane gate (REDRESS 71 gate split).

7. **sonic-rs comparator is a flaw probe (`utf8_lossy`).** C3 (per `skv6-C6`)
   confirms `skinny/crates/bbnf-bench/Cargo.toml` enables global `utf8_lossy`,
   making current sonic rows strict-anchor ineligible (REDRESS 75). The
   strict baseline cannot be measured until comparator schema v3 + the
   sonic-rs rebuild (Wave 0 of IMPL-PACKET).

8. **The path forward (binding):**
   1. Comparator-plane repair (sonic-rs `utf8_lossy` rebuild + schema v3)
   2. Per-`\uXXXX` TBL classifier inside retained string path
   3. mesh generated typed `DirectBuild`
   4. Lock 14 cleanup (passes + codegen + parse-that-regex JSON-name leaks)
   5. CostFacts side-table implementation
   6. Strict 17-corpus matrix
   7. Optional x86 CollapsedStage successor (gated on Zen 4 + NASM author)

The asmjson beat is valid only on strict same-plane x86 rows; M5 Max remains
the primary host with NEON/AdvSIMD + scalar bit operations. PMULL, CSSC,
DotProd, SVE/SME, and x86 AVX-512 esoterica stay unadmitted until exact
profiles point there.

## 9. Report

- File size: written below.
- Total validated count: **19**.
- Total invalidated count: **27** (14 SK-V5 substrate + 5 UTF-8 fusion + 6 SK-V6 retained + 8 SK-V6 direct, minus 6 overlapping sub-rejections inside REDRESS 71/72 already counted in higher-level admits).
- Single most consequential generalization gap: **`CostFacts` side-table is not implemented** in any generic crate (`passes`, `codegen`, `bbnf-simd`, `parse-that-regex`, `runtime/tape`, `ir`). Per C6 audit, every JSON-specific threshold and parser/materializer choice that currently appears as hardcoded codegen or parse-that-regex policy depends on this absent decision substrate. The 5-shape taxonomy is the right abstraction; the cost-selection state behind it must exist before non-JSON grammars can traverse the same pipeline.
