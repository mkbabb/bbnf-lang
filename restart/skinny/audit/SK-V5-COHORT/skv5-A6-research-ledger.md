# SK-V5 A6 Research Ledger — bbnf-lang V9.2 → V9.5 → SK-V4 Consolidation

Date: 2026-05-13
Scope: validated / invalidated / demoted / still-open ledgers, rejected-route table,
per-corpus performance evolution, current load-bearing beliefs.
Source authorities (paths absolute under `/Users/mkbabb/Programming/bbnf-lang/`):

- `skinny/REDRESS.md` (639 LOC; full implementation ledger)
- `skinny/RESULTS.md` (219 LOC; full gate authority)
- `restart/skinny/audit/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md` (353 LOC)
- `restart/skinny/audit/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` (218 LOC)
- `restart/skinny/audit/WAVE-1-2-COHORT-DIGEST.md` (250 LOC)
- `restart/skinny/audit/V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` (332 LOC)
- `restart/HANDOFF.md` (291 LOC)
- `restart/MASTER-PLAN.md` §13 (Tranche H, lines 480–581)
- `skinny/profile/native-sidecars/PROFILE-REPORT.md` (306 LOC)
- `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md` (90 LOC)

Current gate state (binding authority): `skinny/RESULTS.md` records overall
outcome `N-direct / NoGo`. Parse-G rows: `twitter`, `random`, `unicode_mixed`,
`unicode_basic`. Direct pass 6 / 17, red 11 / 17.

Table of contents:

1. Validated items
2. Invalidated items
3. Demoted / narrowed items
4. Still open items
5. Rejected routes ledger
6. Per-corpus performance evolution
7. Per-grammar BackendShape matrix
8. Current load-bearing beliefs
9. PSI / DTA failure-mode anatomy
10. Spec amendment fold-back
11. Falsifiability gates G1-G10
12. Sidecar comparator strictness plane
13. Masking probe ledger
14. Tally
15. Implementation debt snapshot
16. Verification rituals
17. Lock provenance summary
18. Most surprising finding
19. Closing posture

Iteration timeline:

- **V9.2** (2026-05-04 to 2026-05-12 morning): lazy-tape Lock 1 amendment proposed in `LAZY-TAPE-DESIGN.md`; refuted at the eager-token-tape ceiling; lazy-offset implementation landed (~860 LOC).
- **V9.3** (2026-05-12 morning): six-agent comparative-profile cohort. samply skinny + sonic-rs + simdjson; asm dump; DAVID/asmjson research. Architectural lever identified: codegen template shape, not substrate.
- **V9.4** (2026-05-12 afternoon): grand-synthesis cohort. 6 research + 6 profile agents. asmjson + dav1d reference; two-layer vocabulary. READY.
- **V9.5 PSI excavation** (2026-05-12 evening): git history + archive archaeology + failure anatomy; 5+1 canonical Lock 1 modes; the codegen-emitted-FSM recurrence-risk verdict.
- **SK-V3** (2026-05-12 evening into night): 5-shape BackendShape; primitive vocabulary; checkasm gate; Class A/B NEON kernels proposed. GO on SK-V3 minus CollapsedStage.
- **SK-V4** (2026-05-13 morning): N-direct/NoGo honesty; asmjson demoted from M5 Max close; generated SinkOnly mandate. Current receiver packet.
- **SK-V5 A6** (2026-05-13 today): this consolidation ledger.

---

## 1. Validated items

Each row cites the source that ratified the item plus the measurement that closed it.

| Item | Source | Measurement / Evidence |
|---|---|---|
| Lazy/event substrate as right boundary | `skinny/REDRESS.md:224-233`; SK-V4 §3 | Lazy-offset tape (Box<[u32]> offsets + sparse flags) made the historical triad pass and yielded zero JSON payload arena writes/allocations across all 17 corpora (`skinny/RESULTS.md:155-216`, "payload arena counters: 0/0" every row). |
| Codegen overhead separable from substrate ceiling | `restart/skinny/BENCH.md` Track 1/Track 2 contract; SK-V4 §3 | Track 1 (generated) and Track 2 (hand-coded) Mbps stay within 1-3% on every measured row in `skinny/RESULTS.md:5-21` (e.g., twitter 16294 vs 16068, citm 29185 vs 29401). The remaining red rows move together, ruling out a codegen vs. hand-coded gap. |
| Lock 15 i-cache discipline (yyjson evidence) | `restart/skinny/audit/WAVE-1-2-COHORT-DIGEST.md:113-118`; native-sidecars `PROFILE-REPORT.md` §(f) | yyjson hot-leaf count = 1 on every corpus (`yyjson_read_opts` 90.1-97.5% self-time); `parse_value_at` is a single 7304-byte function under `lto=thin codegen-units=1 debug=true` — within the ~20 KiB i-cache budget. |
| Lock 16 primitive admission (escape_mask_64 bug) | `WAVE-1-2-COHORT-DIGEST.md:43-52`; `crates/bbnf-simd/CHECKASM-REPORT.md` | First-run differential parity caught 112/448 alignment-sweep divergences and 2/32 uniform-random divergences in `escape_mask_64` NEON path; root-caused to `new_carry / escaped` state-handoff confusion. Corpora pass 17/17 only because they never trigger the boundary; adversarial inputs do. |
| SIMD vocabulary grammar-neutral + checkasm-gated | `MASTER-PLAN.md:506` (H.W2.5); `WAVE-1-2-COHORT-DIGEST.md:62-72` | Two-layer factoring: Layer 0 `x86inc.asm` (1,978 LOC, BSD-2, dav1d) vendored read-only; Layer 1 `bbnf.asm` (≈600 LOC) authored. First end-to-end primitive `BYTE_CLASS_FROM_EQ_SET_64` lands scalar / aarch64 / x86 / checkasm — commit `9eef728c` (per `REDRESS.md:315-323`). |
| sonic-rs / yyjson-style direct materialization | `REDRESS.md:325-339`; SK-V4 §5 (sonic-rs) | Direct-to-struct sink-only rewrite passed 6/17 rows after duplicate UTF-8 validation cut + `JsonNumberMatch::is_integer` classification. citm 117.0%, apache_builds 110.3%, instruments 122.4%, distinct_values 105.9% of sonic-rs direct (`skinny/RESULTS.md:27-43`). |
| simdjson On Demand iterator model | SK-V4 §5 (simdjson); `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` §3 (simdjson) | The On-Demand cursor model maps to BBNF typed event cursor over retained tape projection. Validated as the correct abstraction; not validated as a parallel structural-index prepass — see Invalidated row. |
| dav1d / FFmpeg process discipline | `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md:137-154`; `WAVE-1-2-COHORT-DIGEST.md:64-72`; SK-V4 §5 (dav1d) | Scalar exec-spec per primitive + per-ISA file naming + checkasm differential gate. Adopted: `bbnf-simd::scalar::*` is the executable spec; `bbnf-simd/tests/checkasm_parity.rs` (516 LOC after strict-mode promotion) is the admission gate. |
| Two-layer ASM vocabulary | `REDRESS.md:315-323`; commit `74406332`; `MASTER-PLAN.md:506` | Layer 0 vendored = ABI / ISA substrate. Layer 1 authored = grammar-neutral `bbnf.asm`. Grammar `.data` tables codegen-emitted, not authored — per-grammar god-modules forbidden in primitive crate (Lock 14). |
| 5-shape BackendShape taxonomy (cost-model-derived) | `WAVE-1-2-COHORT-DIGEST.md:54-61`; `ARCHITECTURE.md` §7.3; `HANDOFF.md:140-159` | `BackendShape ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` exhausts the lowering matrix without new BIR variants. 8-step `derive_backend_shape` algorithm pluggably selects per rule. |
| Expanded corpus over historical triad | `REDRESS.md:31-33`; `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` §1 | Original triad (twitter/citm/canada) would have produced a false green; expanded 17-corpus rollout found `random`, `unicode_mixed`, `unicode_basic`, `unicode_escapes`, `y_string_unicode`, `update_center`, `instruments`, `distinct_values` etc. as the actual blocker shape. |
| Direct-to-struct as required workload | `REDRESS.md:325-339`; SK-V4 §3 | Sink-only timed direct gate landed alongside an untimed retained-view parity oracle. Parse-only wins cannot ratify the V1 direct-to-struct premise; the workload exposed 11 red rows the parse-only gate hid. |

**Validated count: 12.**

### 1.1 Additional measurement provenance per validated item

| Item | Quantified evidence |
|---|---|
| Lazy/event substrate | Twitter 29573 offsets / 118292 logical offset bytes / 1560 sparse flag bytes / 133632 allocated tape bytes (0.21× input). citm 85035 offsets / 524312 allocated (0.30× input). canada 223236 offsets / 1048576 allocated (0.47× input). Zero payload arena writes on all 17 corpora (`RESULTS.md:155-216`). |
| Codegen overhead separable | Twitter T1 16294 vs T2 16068 Mbps (1.4% spread). citm T1 29185 vs T2 29401 (-0.7%). canada T1 16975 vs T2 16675 (1.8%). numbers T1 19195 vs T2 19050 (0.8%). Track 1 and Track 2 move together within Criterion noise on every row. |
| Lock 15 i-cache | yyjson `yyjson_read_opts` 93.2% on twitter / 93.0% citm / 97.2% canada / 94.0% apache_builds / 90.1% github_events / 95.1% update_center / 97.5% unicode_heavy. Hot-leaf count = 1 on every corpus. `parse_value_at` is 7304 bytes ≪ 20 KiB i-cache budget. |
| Lock 16 escape_mask_64 | 112/448 alignment-sweep divergences (25% divergence rate); 2/32 uniform-random divergences (6.25%). Adversarial repro `xorshift seed 0xCAFEF00DBAADF00D, iter 0, 128-byte JSON-pool buffer`. Corpus-clean (17/17 expanded corpora pass). |
| SIMD vocabulary | Layer 0 `x86inc.asm` 1978 LOC BSD-2 (dav1d). Layer 1 `bbnf.asm` ≈600 LOC (per `SOTA-BEAT-DESIGN.md` §5.4). Checkasm differential harness 516 LOC after strict-mode promotion. |
| Direct materialization | citm 25291 / 21615 sonic (117.0%), apache_builds 11083 / 10051 (110.3%), github_events 10595 / 10825 (97.9%), update_center 9140 / 9179 (99.6%), instruments 15877 / 12974 (122.4%), distinct_values 12370 / 11677 (105.9%). 6/17 PASS. |
| dav1d/FFmpeg discipline | Scalar reference per primitive in `bbnf-simd::scalar::*` (executable spec). Per-target file naming `<family>[16]_<isa>.{asm,S}`. `BBNF_SIMD_STRICT=1 cargo test ... checkasm_parity` zero divergences. |
| 5-shape BackendShape | EagerTape / OffsetTape / EventTape / SinkOnly / CollapsedStage. 8-priority `derive_backend_shape` algorithm. Per-grammar matrix entries: JSON 3, CSS L4 4, BBNF-self 3, Sheets 3. |
| Expanded corpus binding | Original triad green; expanded 17-corpus revealed 4 G rows + 9 non-A rows. Direct workload added 11 NO-GO rows beyond parse-only. |
| Direct-to-struct workload | 6/17 PASS (110.3% - 123.7%); 11/17 NO-GO (33.1% - 84.9%). Correctness: `track1=track2=serde`; sonic shape parity. |

---

## 2. Invalidated items

| Item | Source | Measurement / Argument |
|---|---|---|
| Eager retained tape as SOTA-beat substrate | `REDRESS.md:224-233`; SK-V3 §2 row | V9.2 refutation: eager 16-byte tape token plateaued at outcome G against sonic-rs; lazy-offset tape moved the triad to A/A/A. |
| 12-byte token width churn | `REDRESS.md:203-211` | Narrow tokens (kind + flags + start + end) removed `payload_or_skip` and derived skips from spans. Twitter regressed, citm improved, canada within noise — mixed; no clean throughput win, reverted. |
| Pair-token fusion | `REDRESS.md:186-191` | Reduced token count but regressed Track 1 on twitter and canada. Throughput-negative despite count win. |
| PSI / DTA Rust-codegen automaton | `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §1.6, §3 | DTA `dispatch_one` carried 20-35% self-time across every grammar and every input size; "the canonical state-machine-interpreter ceiling." No AW-IV lever amortised it; W2.1 prototype proved beat-sonic-rs achievable without PSI/DTA at all. LLVM cannot fold any per-rule indirection through a runtime dispatch table. |
| StructuralIndex sidecar prepass | `WAVE-1-2-COHORT-DIGEST.md:97-103`; `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` §5 | SOTA-BEAT-DESIGN.md §1 marked HISTORICAL/SUPERSEDED: the retained tape projection IS the structural index. No second parallel substrate. The mask stream is transient. |
| EventCursor as parallel prepass | `WAVE-1-2-COHORT-DIGEST.md:132-142`; `wave2-prototype/PROTOTYPE-REPORT.md` | Scoped-down prototype regresses 0.63×-0.89× vs V9.4 baseline across six corpora (random 1280 vs 1641 Mbps; update-center 1559 vs 2481; twitter 2194 vs 3152). Refuted the 0.18-0.22 c/B projection at this level. Event cursor must be the lowering boundary, not a sidecar in front of unchanged `parse_value_at`. |
| Function-pointer dispatch table | `REDRESS.md:193-201`; `RESULTS.md:52,58,64,...` ("INVALID duplicate-probe disabled; real function-pointer table regressed") | Original `alternate_dispatch_table_plan` probe duplicated canonical Track 1. Real 256-entry function-pointer table was implemented, measured, regressed key corpora, reverted. Match dispatch stays canonical. |
| Capacity prescan (Plan A / B / C) | `wave2-capacity/CAPACITY-REPORT.md`; `WAVE-1-2-COHORT-DIGEST.md:120-131`; `REDRESS.md` Plan D adoption | Plan A (sampled heuristic) over-reserves 2.53×, overfit to update-center 4 KiB prefix. Plan B (full scalar pre-scan) 2.3× throughput hit. Plan C (one-shot SIMD pre-scan) 120 µs/parse cost, discards position vector. Plan D (grow-only, initial 256, geometric doubling) wins +4.8% random / +10.2% github_events; 23-64% capacity reclamation. |
| Generic SWAR whitespace skipper | `REDRESS.md:268-274`, `442-448`; `WAVE-1-2-COHORT-DIGEST.md:104-118` | A separate whitespace-bearing parse index doubled twitter Track 1 parse time. Was diagnosed as the gap; real diagnosis is two pathology classes (tiny-string scalar loop + `\uXXXX` hex decode), neither is whitespace re-scan. |
| Separator elision | `REDRESS.md:268-274`, `622-625` | Measured or audited; not retained; failed to beat the direct hot-path changes (delim fusion + SWAR digit runs). |
| `raw.parse::<f64>()` shortcut | `REDRESS.md:341-357` | Direct fast path tested; `canada` exposed float-shape mismatch against the serde oracle. Parity failure → rejected. |
| Active 16-byte tiny-string routing | `REDRESS.md:301-313` | When the 16-byte tiny-string helper was routed into Track 1/Track 2, Criterion showed a real `twitter` regression of roughly 25% on both tracks. Kernel is admitted (checkasm-green) but mis-routed; active parser remains on 8-byte scalar tiny recognizer. |
| asmjson as M5 Max close condition | SK-V4 §4; `native-sidecars/PROFILE-REPORT.md` §a footnote † | asmjson's 10.93 GiB/s headline is x86_64 AVX-512BW only. On M5 Max the runnable path is SWAR (3315/2447/669 MiB/s on synthetic corpora). M5 close is against sonic-rs / simdjson / yyjson; asmjson-class CollapsedStage is the x86 successor route. |
| Primitive-only work without same-wave consumer | SK-V4 §3 (Lock 16); `LESSONS-LEARNED.md:17-26`; V9.5 PSI §1.3 | Lock 1 lesson: "every substrate change must land with a same-wave consumer or an explicitly declared brittleness window and restoration wave." AX Proposition 4: novel levers compound only when they share a substrate AND a demonstrable floor. |
| Eager token stream as SOTA-beat substrate | `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` §2 row 1; SK-V4 §4 | Eager token stream hit a measured ceiling and lost to lazy-materialization competitors. Remains valid for recovery / layout / parse-time host decode / overlapping first sets. |
| Cost model as aspirational | SK-V4 §4 | Materialization shape, capacity policy, primitive selection, generated direct sink, and exact scalar materialization are load-bearing — not aspirational tuning. |
| Dispatch table as the missing win | SK-V4 §4; `REDRESS.md:193-201` | First row was an accidental duplicate; the real function-pointer table regressed. LLVM-owned `match`/jump-table lowering stays canonical outside admitted `CollapsedStage` NASM. |
| Primitive parity as sufficient | SK-V4 §4 (last row of §4 invalidated table) | A primitive can be correct (checkasm-green) and still not help if the call shape, inlining, or workload route is wrong. Class A/B kernels admitted; current gate still `N-direct / NoGo`. |

**Invalidated count: 18.**

### 2.1 Additional measurement provenance per invalidated item

| Item | Quantified evidence |
|---|---|
| EventCursor scoped-down prototype | random 1280 vs 1641 Mbps baseline (-22%); update-center 1559 vs 2481 (-37%); twitter 2194 vs 3152 (-30%); citm-class corpora similar. 6/6 corpora regressed. |
| Plan A capacity (sampled heuristic) | Over-reserves 2.53× (Plan D at 1.87×); overfit to update-center 4 KiB prefix. |
| Plan B capacity (full scalar pre-scan) | 2.3× throughput hit on parse. |
| Plan C capacity (one-shot SIMD pre-scan) | 120 µs/parse cost. Position vector discarded after capacity computation. |
| Active 16-byte tiny-string routing | ~25% twitter regression on both Track 1 and Track 2 when 16-byte helper routed into the parser. Kernel itself parity-correct. |
| `raw.parse::<f64>()` shortcut | `canada` exposed float-shape mismatch against serde oracle on parity check. |
| Function-pointer dispatch table | Reverted after key corpora regressed. INVALID across all 17 corpora in `RESULTS.md`. |
| Pair-token fusion | Token count reduced; Track 1 regressed on twitter and canada. |
| 12-byte token width | Logical tape bytes saved; twitter regressed, citm improved, canada within noise. Mixed → reverted. |
| Eager whitespace-bearing parse index | Twitter Track 1 parse time doubled (~783 µs). |
| Duplicate structural-byte column | +3-6% Track 1/Track 2 on six rows when removed. |
| Generic SWAR whitespace skipper | No improved expanded gate after admission. Diagnostic mis-attribution: the real pathology is Class A tiny-string scalar + Class B `\uXXXX` hex decode. |
| PSI / DTA dispatch_one | 20-35% self-time across every grammar and every input size. W2.1 prototype proved beat-sonic-rs without DTA at all. |
| StructuralIndex sidecar | Marked HISTORICAL/SUPERSEDED in SOTA-BEAT-DESIGN.md §1. Retained tape projection IS the structural index. |
| Eager 16-byte tape token (substrate) | Plateaued around prior outcome G ceiling; lazy-offset triad pass closed it for the historical 3 corpora. |
| asmjson as M5 Max close | AVX-512BW only; M5 Max SWAR path 3315/2447/669 MiB/s on synthetic, comparable to skinny v3. |
| Cost model as aspirational | Materialization shape + capacity policy + primitive selection + generated direct sink + exact scalar materialization all load-bearing — measured as decisive in each ledgered route. |
| Primitive parity as sufficient | Class A 16-byte tiny-string kernel admitted (checkasm-green) but routing regressed twitter 25%. |

---

## 3. Demoted / narrowed items

Items validated for one scope but invalidated for another.

| Item | Valid scope | Invalid scope | Source |
|---|---|---|---|
| asmjson DPDA architecture | Valid x86_64 AVX-512 successor route; 9-state finite control + hardware-bounded explicit stack (`open_buf[MAX_JSON_DEPTH=64]`); direct-threaded dispatch via `r10`; tzcnt-driven seek; EOB padding. | Invalid as ARM / M5 Max target; AVX-512BW only. Invalid as the close gate for SK-V3 / SK-V4. Demoted to deferred successor tranche. | SK-V4 §5 (asmjson); `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §3 Risk A; `MASTER-PLAN.md:515` (per-grammar × ISA authoring waves). |
| CollapsedStage shape | Valid as taxonomic value (one of 5 BackendShape values); cost-model 8-priority derivation correctly produces it when target features admit AND rule is a hub with ≥4 byte-disjoint arms AND no `@error(recover)` AND no `@host fn` parse-time-decoded AND no `@layout`. | Invalid as Rust codegen output — codegen-emitted FSM derived from Grammar IR is the same risk-shape as AW-V's auto-derive thesis (W3 → lost by W6). Only NASM-author-per-grammar admissible; never an LLVM target. | `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §3 (Risk A/B/C); SK-V4 §6 (CollapsedStage = hand-authored per-grammar DPDA kernel on admitted ISA); `REDRESS.md:633-639` (per-grammar NASM authoring route guarded by `BBNF-COLLAPSEDSTAGE-NOT-VIABLE`). |
| StructuralIndex | Valid as runtime intermediate concept: the retained tape projection IS the structural index (offsets + sparse flags + payload arena as one structure, three arrays carried together). | Invalid as parallel substrate / sidecar prepass / second authoritative tree. | `WAVE-1-2-COHORT-DIGEST.md:97-103`; `GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` §5; `SUBSTRATE.md:217`. |

---

## 4. Still open items

Items not yet validated or invalidated. Each row carries the SK-V4 packet wave or research artifact pointing to the next experimental step.

| Item | Status | Where it sits |
|---|---|---|
| Eisel-Lemire number materializer | OPEN — required for direct sink float parity | SK-V4 packet Wave 2 (`HANDOFF.md:200`); reassay direct-sink profile shows `serde_json::parse_number` 23.4% self-time on `numbers` (`reassay-skv4-2026-05-13/PROFILE-REPORT.md:34`). |
| Class A NEON `match_tiny_plain_string` kernel | AUTHORED + checkasm-green; AWAITING WIRING | `MASTER-PLAN.md:505` (H.W2); `REDRESS.md:301-313` (current routing regressed 25% on twitter — kernel parity-correct but call shape wrong). |
| Class B NEON `\uXXXX` TBL hex decode | AUTHORED + checkasm-green; AWAITING WIRING | `WAVE-1-2-COHORT-DIGEST.md:161-172`; kernel exists in `bbnf-simd/aarch64/`, awaits consumer in `unescape_json_string`. |
| Generated SinkOnly from BIR DirectBuild | OPEN — Track 1 direct still calls a bench-private parser | `REDRESS.md:118-124` (BIR carries `SpanMark / TapeEmit / DirectBuild / Return`, but codegen still lowers through skinny fixed template); SK-V4 packet Wave 1 (`HANDOFF.md:199`). |
| Parse hot-hub PC-level attribution | OPEN — `parse_value_at` shows 99.5-99.7% self-time symbol-level | `reassay-skv4-2026-05-13/PROFILE-REPORT.md:40-45` ("no-inline or PC-level attribution is needed"); SK-V4 packet Wave 3. |
| EventCursor as lowering boundary | OPEN — scoped-down version refuted; full design pending | `WAVE-1-2-COHORT-DIGEST.md:215-224` ("does the *full* prototype recover the 0.18-0.22 c/B projection?"); provisional code at `runtime/src/grammars/json/generated_eventcursor.rs` (360 LOC, `eventcursor` feature). |
| 8 remaining bbnf.asm primitive bodies | OPEN | `MASTER-PLAN.md:506` (H.W2.5) names 9 macros; `BYTE_CLASS_FROM_EQ_SET_64` lands first; remaining: `BYTE_CLASS_FROM_TABLE_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`, `EOB_PAD_CLAMP`, `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`. |
| x86 CollapsedStage successor | OPEN — conditional on Zen 4 silicon + parity harness + per-grammar firing audit | `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §4.2 / §4.3; SK-V4 packet §8 (Wave 6b deferred). |
| Per-grammar CollapsedStage authoring (separate wave per grammar × ISA) | OPEN — cadence grammar-arrival-driven, not in H tranche | `MASTER-PLAN.md:515` (each grammar whose `LayoutFacts.backend_shape` admits CollapsedStage acquires one wave per (grammar × ISA) pair). |
| AVX-512 5-pack (k-mask arith / VPCLMULQDQ-512 / AVX-IFMA / VNNI / BITALG / GFNI) | OPEN — admitted to Lock 16 allowlist; consumer wiring deferred | `MASTER-PLAN.md:534-547`; "land first as grammar-neutral primitives consumed by OffsetTape / EventTape / SinkOnly hot loops through scalar/checkasm-gated FFI shims" (Wave 6a unconditional once x86_64 dev access exists). |
| 3-pack M5 Max NEON esoterica (LD4-interleaved, BCAX/EOR3, svmatch_u8 emulation) | OPEN — admitted to Lock 16; consumer wiring deferred | `MASTER-PLAN.md:530-532`; LD4 ~10% drop in simdjson stage1 c/B on Apple arm64; BCAX/EOR3 ~12-18% inner-loop op-count reduction; svmatch_u8 binds NEON body of `BYTE_CLASS_FROM_EQ_SET_64`. |
| Full EventCursor prototype (mask + LUT + ptr/end sentinel + cold-path inline) | OPEN | `WAVE-1-2-COHORT-DIGEST.md:215-224`; "the full prototype is in flight — the refutation specifically falsifies the scoped-down shape, not the full design"; "needs the complete bounds-elision + cold-path discipline, not just the LUT". |
| Plan C capacity re-examination post-event-cursor | OPEN — re-examination ticket filed against post-Wave-1 re-baseline | `wave2-capacity/CAPACITY-REPORT.md` §4; "once Wave 1's `attach_structural_index` produces the structural-index as the event-cursor input, plan C's pre-scan cost becomes free". |
| SME (Scalable Matrix Extension, M4+) prototype | DEFERRED — silicon access pending | `WAVE-1-2-COHORT-DIGEST.md:228-231`; AMX ruled out (matrix-domain, not byte-class); SME has cleaner load path but Apple Silicon SME silicon not yet in dev fleet. Tranche-after-next probe; not on SK-V3/V4 close gate. |
| Zen 4 silicon access for asmjson-class measurement | DEFERRED — unavailable | `WAVE-1-2-COHORT-DIGEST.md:232-241`; native-sidecars asmjson Zen 4 AVX-512 row at 11192 MiB/s is published anchor only. Wave 6 (x86_64 strict SOTA path) gates on Zen 4 silicon access. |
| Differential parity harness for FSM codegen | OPEN — required before any Phase 4 dispatch | `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §4.2 / Gate G7; `tests/fsm_codegen_parity.rs` does not exist. Existence is a hard precondition for any CollapsedStage runtime call site. |
| Per-grammar `derive_backend_shape` firing audit | OPEN — Gate G6 / G8 | `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §4.2.2 / §7.3; audit table per grammar must show ≥2 shapes firing across the 9-grammar matrix, else taxonomy collapses on AQ.5 precedent. |
| Workload matrix expansion (`parse_full_traversal` / `path_lookup` / `unicode_string_float` / `memory` / `cycles_per_byte`) | OPEN — currently only `parse_only` + `direct_to_struct` measured | `MASTER-PLAN.md:508` (H.W4); SK-V3 packet §7 workload matrix. |
| Strictness plane separation (strict vs permissive comparators) | OPEN | SK-V4 §5 (asmjson "what not to copy"); strict comparison demotes asmjson's headline. Sidecar comparators must split strictness/permissive rows. |

---

## 5. Rejected routes ledger (from `skinny/REDRESS.md`)

Systematic walk of named rejected routes; each row carries the REDRESS section reference + one-line justification + measurement.

| Route | REDRESS § | Measurement / justification |
|---|---|---|
| Skipless 12-byte tape tokens | 18 (`:203-211`) | Removed stored `payload_or_skip` column; derived subtree skips from spans at view-traversal time. Twitter regressed, citm improved, canada within noise. No clean throughput win → reverted. |
| Function-pointer dispatch table (256-entry) | 17 (`:193-201`) | Old probe row duplicated canonical Track 1 (reported as INVALID). Real function-pointer table regressed key corpora vs Rust `match` LLVM jump-table lowering. |
| Pair-token fusion | 16 (`:186-191`); Final summary `:583-584` | Pair-token-free object projection reduced token count but regressed Track 1 on twitter and canada. Token-count win ≠ throughput win. |
| Structural-index typed parser prepass | 25 (`:268-274`); §17 of "Hitherto Documented Changes" | Bench-grade structural-only scan is split from parse-index scan; duplicate structural-byte column measured throughput-negative and removed (+3-6% Track 1/Track 2 on six rows). Sidecar typed-parser prepass invalidated. |
| Eager whitespace-bearing parse index | §5 of "Skinny Spec Amendments Folded" (`:442-448`) | Twitter parse time doubled to ~783 µs when measured. Spec amendment forbids forcing every parser scan to classify and emit whitespace bytes. |
| Generic SWAR whitespace skipper | §25 (`:268-274`); summary `:622-625` | Diagnostic was wrong: real pathology is Class A tiny-string scalar loop + Class B `\uXXXX` hex decode, not whitespace re-scan. |
| NEON no-escape string matcher | §25 (`:268-274`); summary `:622-625` | Measured or audited; did not retain. No improved expanded gate. |
| Separator elision | §25 (`:268-274`); summary `:622-625` | No benefit measured; rejected. |
| Eager parse-time string decode (`decode_json_string_to_arena`) | §19 (`:213-221`); summary `:589-591` | Host-call dispatch overhead probe passes (sub-ns to ~1ns); eager-decode rows are MASKING (>1.10× T1 on every corpus per `RESULTS.md`). V1 JSON can only claim host-fn-free faithfulness if decode stays lazy. |
| Sampled-heuristic capacity (Plan A) | §22-§23 (`:244-256`); `wave2-capacity/CAPACITY-REPORT.md` | Over-reserves 2.53×; overfit to `update-center` 4 KiB prefix. Plan D wins +4.8% / +10.2%, reclaims 23-64% capacity. |
| Full scalar pre-scan capacity (Plan B) | wave2-capacity | 2.3× throughput hit; rejected. |
| One-shot SIMD pre-scan capacity (Plan C) | wave2-capacity | 120 µs/parse cost; discards position vector. Re-examination ticket against post-event-cursor re-baseline. |
| Active 16-byte tiny-string routing | §28 (`:301-313`) | Routing into Track 1/Track 2 produced ~25% twitter regression on both tracks. Kernel admitted; routing rejected. |
| `raw.parse::<f64>()` direct fast path | §31 (`:341-357`) | `canada` exposed float-shape mismatch against serde oracle. Parity failure. |
| EventCursor scoped-down sidecar prototype | summary `:622-625`; `wave2-prototype/PROTOTYPE-REPORT.md` | 0.63×-0.89× regression across six corpora; refuted the 0.18-0.22 c/B projection at the scoped-down shape. |
| Eager 16-byte tape token (canonical substrate) | §20 (`:223-233`); §22-§23 | Lazy-offset tape replaced it; original twitter Track 1 14810 Mbps post-migration moved through sparse-flag and hot-path wins to the triad pass. |
| Inlined retained-view direct walk (timed) | §30 (`:325-339`) | Sink-only digest replaced the timed retained-view walk; view walk now untimed parity oracle only. Direct throughput roughly doubled/tripled. |
| Duplicate structural-byte column on parse index | §2 (`:79-86`) | Removed; improved all six Track 1/Track 2 parse rows by ~3-6%. |
| Hardcoded payload counters in bench metadata | §8 (`:127-132`) | Replaced with measured counters; gate fails schema validation for bbnf rows whose counters are missing or non-zero. |
| `alternate_dispatch_table_plan` row | §10 (`:140-148`), §17, §27 | Original duplicated canonical Track 1; now marked INVALID across all corpora in `RESULTS.md:52,58,...`. |

**Rejected-route count: 20.**

Cross-cutting rejection patterns:

- **Width perturbation**: 12-byte token, separator elision — token-economy tuning that cannot beat the lazy-offset substrate change.
- **Sidecar / parallel substrate**: structural-index typed parser prepass, EventCursor scoped-down prototype, eager whitespace-bearing parse index — every parallel-prepass shape regressed or duplicated work already in the retained tape projection.
- **Capacity prescan**: Plans A/B/C — only Plan D (grow-only geometric) wins, because the parse hot path cannot afford pre-scan cost or sampled-heuristic over-reservation.
- **Dispatch indirection**: 256-entry function-pointer table, alternate_dispatch_table_plan — the canonical PSI/DTA failure-mode signature; LLVM `match` jump-table is the only viable lowering outside admitted CollapsedStage NASM.
- **Eager scalar materialization**: `raw.parse::<f64>()` shortcut — every eager-materialize shortcut at parse time has cost or parity failures; lazy-decode-on-view stays canonical.
- **Active-routing of correct primitives**: 16-byte tiny-string routing — checkasm parity is necessary but not sufficient; call shape and inlining are part of the consumer contract.

---

## 6. Per-corpus performance evolution

### 6.1 The historical triad (twitter, citm, canada)

| Stage | twitter (Mbps) | citm (Mbps) | canada (Mbps) | Notes |
|---|---:|---:|---:|---|
| Eager 16-byte tape token | ~outcome G | ~outcome G | ~outcome G | Plateaued; lost to lazy-materialization competitors (`REDRESS.md:223-233`). |
| Post lazy-offset migration | 14810 (T1) | n/a | n/a | Still outcome G immediately post-migration (`REDRESS.md:230-233`). |
| + sparse flags + spare-capacity offset writes | improved | improved | improved | Twitter sparse 1560 flag bytes, citm 5, canada 0 (`RESULTS.md:65-67`). |
| + cold errors + SWAR digit runs + SWAR plain-string + delim fusion + newline-indent skip + `parse_value_at` + short plain-string fast path + Track 2 inline parity | triad PASS | triad PASS | triad PASS | "Parser hot-path wins landed without changing the substrate contract" (`REDRESS.md:258-266`). |
| Current expanded gate (`RESULTS.md:5-7`) | 16294 / 78.3% sonic | 29185 / 117.2% sonic | 16975 / 134.1% sonic | twitter remains G/NoGo on expanded gate; citm and canada are A/GO. |

### 6.2 The expanded 17-corpus rollout

Outcome distribution after SK-V3 Wave 0/1 implementation pass (current `RESULTS.md`):

| Outcome | Corpora |
|---|---|
| A / GO | citm_catalog, canada, apache_builds, github_events, mesh, gsoc-2018, marine_ik, numbers |
| C / GO | update_center, instruments, unicode_escapes, distinct_values, y_string_unicode |
| G / NO-GO | twitter, random, unicode_mixed, unicode_basic |

The 4 G rows are the binding parse-G blocker. The 4 + 5 = 9 non-A rows are the parse SOTA-BEAT residue.

What improved during expansion:
- citm 29185 Mbps (117.2% of sonic-rs), canada 16975 (134.1%), numbers 19195 (141.5%), marine_ik 13240 (131.6%), apache_builds 17734 (109.4%), github_events 25332 (114.2%), gsoc-2018 47481 (109.9%).

What regressed / stayed red during expansion:
- random 7770 (50.6%), unicode_mixed 7384 (46.5%), unicode_basic 6561 (49.3%), unicode_escapes 13945 (86.9%), twitter 16294 (78.3%).

### 6.3 Cross-parser landscape on M5 Max (MiB/s)

From `native-sidecars/PROFILE-REPORT.md` §a + `HANDOFF.md:97-117`. Cells in **bold** are corpora where skinny v3 leads the comparator.

| Corpus | skinny v3 | yyjson inlined | simdjson C++ inlined | sonic-rs | asmjson SWAR | asmjson Zen 4 AVX-512 anchor |
|---|---:|---:|---:|---:|---:|---:|
| twitter | **2631** | 3687 | 2923 | 2415 | 3315† | 11192‡ |
| citm | **3571** | 2498 | 4270 | — | 2447† | 11192‡ |
| canada | **1675** | 1550 | 1370 | — | — | — |
| apache_builds | 1850 | 1940 | **4293** | — | — | — |
| github_events | 2267 | 2554 | **4725** | — | — | — |
| update_center | 1763 | 2210 | **3647** | — | — | — |
| mesh | **1194** | — | 1122 | — | 669† | — |
| random | 1117 | — | **2460** | — | 669† | — |
| distinct_values | 1927 | — | **2721** | — | — | — |
| unicode_basic | 1731 | — | **1940** | — | — | — |
| unicode_mixed | **1719** | 1228 | 1568 | — | — | — |
| unicode_escapes | 587 | — | **672** | — | — | — |
| y_string_unicode | 865 | — | **1624** | — | — | — |

† M5 Max native arm64 SWAR against asmjson synthetic corpora (`string_array` / `string_object` / `mixed`).
‡ Published Zen 4 AVX-512BW DOM anchor (10.93 GiB/s = 11192 MiB/s); cross-architecture target.

skinny v3 leads on 4/17 corpora vs simdjson C++ / yyjson on M5 Max:
- **citm** 3571 MiB/s — +43% vs yyjson on the largest mixed-shape corpus.
- **canada** 1675 MiB/s — +22% vs simdjson DOM, +8% vs yyjson; number-heavy.
- **mesh** 1194 MiB/s — +6% vs simdjson DOM; float-array body.
- **unicode_mixed** 1719 MiB/s — +10% vs simdjson DOM; classifier elides UTF-8 validator entirely.

twitter at 2631 MiB/s is +9% vs sonic-rs and −10% vs simdjson C++; remaining gap is the cost of staying in safe Rust with lazy-decode shape.

### 6.4 Hot-leaf attribution profile (post-SK-V4 reassay)

From `reassay-skv4-2026-05-13/PROFILE-REPORT.md`:

Direct sink, Track 1 hot leaves:

| Corpus | Top self-time | Reading |
|---|---|---|
| twitter | `SinkParser::string` 62.3%, `SinkParser::value` 19.8%, `SinkParser::object` 16.3% | String-bound |
| numbers | `SinkParser::value` 62.1%, `serde_json::parse_number` 23.4%, `serde_number_digest` 12.6% | Exact number materialization is the blocker |
| unicode_mixed | `SinkParser::string` 81.4%, `SinkParser::value` 12.7%, `SinkParser::object` 3.0% | Unicode/string decode quality dominates |
| random | `SinkParser::string` 65.1%, `SinkParser::value` 19.1%, `SinkParser::object` 15.8% | String-bound |

Parse-only lazy path:

| Corpus | Top self-time | Reading |
|---|---|---|
| twitter | `parse_value_at` 99.7% | Symbol-level too fused; no-inline / PC-level attribution needed |
| numbers | `parse_value_at` 97.2%, `_platform_memmove` 1.3%, `TapeBuilder::new` 0.7% | Allocation no longer top cause |
| unicode_mixed | `parse_value_at` 99.5% | String-leaf detail hidden |
| random | `parse_value_at` 99.6% | Hot hub must be broken apart |

### 6.5 Direct workload introduction

After the sink-only rewrite (`REDRESS.md:325-339`) and the duplicate UTF-8 validation cut + `JsonNumberMatch::is_integer` classification:

| Bucket | Direct rows | Performance against sonic-rs direct |
|---|---|---|
| PASS (6/17) | citm_catalog, apache_builds, github_events, update_center, instruments, distinct_values | 110.3% - 123.7% |
| NO-GO (11/17) | twitter, canada, mesh, random, gsoc-2018, marine_ik, numbers, unicode_mixed, unicode_escapes, unicode_basic, y_string_unicode | 33.1% - 84.9% |

The 11 NO-GO rows concentrate in numeric (canada 40.8%, numbers 33.1%, mesh 51.9%), Unicode (unicode_mixed 49.9%, unicode_escapes 50.4%, unicode_basic 68.5%, y_string_unicode 65.8%), and dense retained-sink shapes (gsoc-2018 67.2%, marine_ik 73.4%).

Each step, what improved + regressed:

| Step | Improved | Regressed / unchanged |
|---|---|---|
| Sink-only digest path (eliminate retained-view walk in timed) | Direct throughput roughly 2-3× | None |
| Remove duplicate UTF-8 validation post `match_json_string_at_quote` | citm, apache_builds, github_events, update_center, instruments, distinct_values (6 PASS rows) | numeric / Unicode rows |
| Move integer/non-integer classification into `JsonNumberMatch::is_integer` | integer-heavy rows | float-heavy `canada`, `numbers` |
| `raw.parse::<f64>()` fast path (tested, rejected) | nothing — parity failed on `canada` | rolled back |

---

## 7. Per-grammar BackendShape matrix (Lock 14 generalisation)

From `HANDOFF.md:163-176`. The same cost-model derivation that selects per JSON rule applies across the V1 grammar fleet. No per-grammar code in any generic crate.

| Grammar | Rule | Backend shape | Rationale |
|---|---|---|---|
| JSON | `value` | `OffsetTape` | Byte-disjoint dispatch alphabet (`{`, `[`, `"`, `t`, `f`, `n`, `-`, digit); lazy scalar spans; no recovery; no layout. |
| JSON | `string` (skinny) | `OffsetTape` | Quote-bounded; HAS_ESC flag captured at scan; raw span emitted; decode is view-time. |
| JSON | `string` (V1) | `EagerTape` | V1 default if `@host fn decode` activates at parse; reverts to `OffsetTape` under skinny opt-in. |
| CSS L4 | `ruleItem` | `OffsetTape` | Byte-disjoint dispatch (`@`, ident-start, `.`, `#`, `*`); no recovery on hot ruleItem dispatch hub. |
| CSS L4 | `value` | `EagerTape` | Dimension/percentage/function-call distinction needs source-byte lookahead inside type-system arm. |
| CSS L4 | `declaration` | `EventTape` | Payload class (property name → typed-property enum) MUST be retained for visitor traversal. |
| CSS L4 | `selector` | `EagerTape` | Complex/compound/simple selector boundary needs source-byte lookahead; recovery scope. |
| BBNF-self | `grammar` / `declaration` / `term` | `OffsetTape` | Byte-disjoint top-level dispatch (`@`, ident, `(`); lazy span over rule body. |
| BBNF-self | `expression` | `EagerTape` | Pratt operator chain requires precedence climbing on source bytes; auto-detected per Lock 10. |
| BBNF-self | `directive` | `EventTape` | Payload class (directive kind → enum variant) carries through to LayoutFacts consumption. |
| Sheets | `formula` / `cellRef` / `primary` | `OffsetTape` | A1-notation byte-disjoint dispatch; lazy span over reference body. |
| Sheets | `function` / `arrayLiteral` | `EventTape` | Function-name payload (`LET(`/`LAMBDA(` prefix-DFA discriminator) must reach typed AST. |
| Sheets | `expression` | `EagerTape` | Infix-operator precedence requires source-byte lookahead. |

**Cross-grammar feedback loop**: a NEON kernel that closes JSON `match_tiny_plain_string` IS the kernel that closes CSS L4 ident-token scan, BBNF-self ident-token scan, and Sheets cell-reference scan. The `bbnf-simd` primitive crate exposes grammar-neutral `StringMode`/`NumberScan`/`ByteClassPlan`/`KernelSet`. Grammar names appear only in generated runtime modules; the primitive crate carries ZERO grammar-specific code per Lock 14.

---

## 8. Current load-bearing beliefs (post-SK-V4)

bbnf-lang's current architectural beliefs about JSON parsing and grammar parsing in general.

### Substrate

The substrate boundary is:

```
bytes
  -> scan / mask producer
  -> typed event cursor
  -> { OffsetTape | EventTape | SinkOnly | CollapsedStage | EagerTape fallback }
```

The retained tape projection IS the structural index. There is no second parallel substrate. The mask stream is transient; if retained, it is the tape projection.

- `OffsetTape` — retained offsets; payloads lazy. Default for byte-disjoint dispatch alphabets without recovery / layout / host decode.
- `EventTape` — retained event cells when recovery / layout / payload side facts must survive (CSS `declaration`, BBNF `directive`, Sheets `function`).
- `SinkOnly` — direct typed writes; no retained document identity. Selected for direct-only public output mode.
- `CollapsedStage` — hand-authored per-grammar DPDA kernel on admitted ISA. Never an LLVM target; only NASM author per (grammar × ISA). Cost model selects it; admission gated by `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` diagnostic.
- `EagerTape` — source-byte recursive descent fallback for recovery, layout, parse-time host decode, or first-set overlap.

### Lowering

- No new BIR variant. The 20-variant BIR alphabet (`ARCHITECTURE.md` §7.2) is preserved verbatim.
- No new BBNF directive. No `@runtime` / `@simd` / `@backend` / `@shape` / `@asm`.
- `Alt { Dispatch }` lowers to up to five access patterns based on `LayoutFacts.backend_shape`, which is one side-table field with one 8-priority derivation algorithm at `passes::recognizers::derive_backend_shape`.
- Event cursor is the lowering boundary, not a parallel prepass. The cursor advances over offsets; source-byte rescans live only inside `parse-that/string`, `parse-that/number`, `parse-that/unicode`.
- Cost model is load-bearing: materialization shape, capacity policy (Plan D grow-only), primitive selection, generated direct sink, and exact scalar materialization all flow from it.

### Primitives

- Two-layer vocabulary. Layer 0 `x86inc.asm` / `x86util.asm` vendored read-only from dav1d (1,978 LOC, BSD-2). Layer 1 grammar-neutral `bbnf.asm` (≈600 LOC) authored.
- Grammar tables live in generated `.data`, not in the primitive crate. The primitive crate carries ZERO grammar-specific code per Lock 14.
- Every primitive: scalar reference in `bbnf-simd::scalar::*` (executable spec) + per-ISA body (`scalar/aarch64/x86_64`) + checkasm differential parity test before any consumer wiring.
- Lock 16 admissibility allowlist: every `core::arch::*` use-site and every `asm!` block traces to a Lock 16 row with citation + checkasm parity admission. Hand-tuned undocumented intrinsic loops without an architectural name are forbidden.
- Same-wave consumer required: no primitive lands without a wave-bound consumer or an explicitly declared brittleness window (Era V signature).

### Workloads

- Expanded 17-corpus gate is binding. Historical triad pass is real substrate evidence but not the close condition.
- Direct-to-struct is a throughput gate, not just a correctness proof. Bench-owned sink parsers are not Track 1; Track 1 must call generated runtime `SinkOnly` from BIR `DirectBuild`.
- Workload matrix: `parse_only`, `parse_full_traversal`, `path_lookup`, `direct_to_struct`, `unicode_string_float`, `memory`, `cycles_per_byte`.
- Cold per-parse only. Warm/cached benchmarks are disingenuous. Peak RSS sampled in one-shot subprocess probes for bbnf and the fastest S anchor.

### Gates

- Lock 1: tape ∪ direct-to-struct as ONE substrate with five projections. No parallel substrates. No OpenFrame ladders.
- Lock 5: V1 ships `RustBackend: Backend` only; WASM defers post-V1.
- Lock 10: Pratt and SIMD are auto-detected; cost model selects backend shape.
- Lock 14: full grammar generalisation; zero overfitting. No grammar names in generic crates.
- Lock 15: `[profile.release] lto="fat" codegen-units=1 panic="abort" debug=true`; force-inline on mined hot rules; i-cache budget ≈ 20 KiB (yyjson evidence: hot-leaf count = 1 on every corpus).
- Lock 16: SIMD/ASM admissibility allowlist; primitive admission discipline (scalar ref + checkasm + same-wave consumer).
- Outcome verdicts: `A/GO`, `C/GO`, `G/NoGo`, `N-direct/NoGo`. Current full gate is `N-direct / NoGo`.
- Falsifiability gates G1-G10 wired into CI and wave-exit gates per `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §8.

### Comparator posture

- M5 Max close: against sonic-rs / simdjson C++ / yyjson on host. skinny v3 already leads on 4 of 17 corpora (citm +43% vs yyjson; canada +22% vs simdjson DOM + 8% vs yyjson; mesh +6% vs simdjson; unicode_mixed +10% vs simdjson). Source: `native-sidecars/PROFILE-REPORT.md` §c.
- asmjson on M5 Max is SWAR-only (3315/2447/669 MiB/s on synthetic). asmjson's 10.93 GiB/s anchor is Zen 4 AVX-512BW + 9-state DPDA architecture, not 512-bit width esoterica (`WAVE-1-2-COHORT-DIGEST.md:31-38`).
- asmjson is NOT a pure FSM — it's a DPDA with `frames_buf[MAX_JSON_DEPTH=64]` + `open_buf[64]` hardware-bounded explicit stack. "9-state FSM" framing was amended to "9-state DPDA" per `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §4.1 Amendment 4.
- Strictness plane separates strict and permissive comparators. asmjson accepts control characters as whitespace and does not fully scan strings for unescaped controls per its own docs; strict comparison demotes the headline (SK-V4 §5).
- yyjson's noinline self-time decomposition: `read_str_opt` 14-41%, `read_root_pretty` 14-30%, `byte_match_2` (structural-byte scanner) 11-23%, `char_is_ascii_skip` 8-22%, `read_num` 3-30% (`native-sidecars/PROFILE-REPORT.md` §f). Confirms that scalar fusion (Lock 15) is as load-bearing as SIMD (Lock 16).
- simdjson stage1 vs stage2 ratio inverts on escape-heavy corpora: unicode_escapes 8.8% / 60.7% / 30.5%. SOTA-BEAT bench must classify by workload, not average.

### Authoring discipline

- No new BBNF directive. No `@runtime` / `@simd` / `@backend` / `@shape` / `@asm`. Verification grep returns zero.
- No new BIR variant. The 20-variant alphabet is preserved verbatim.
- No new grammar surface. Cost model auto-derives backend shape from existing Grammar IR facts.
- Every wave dispatch carries explicit hard cap (research 20 min, plan 15 min, redress 30 min) per `feedback_dispatch_hard_cap`.
- Triumvirate auto-trigger on JSONL quiet > 15min or first-pass no-commit per `feedback_triumvirate_auto_trigger`.
- Triumvirate roles are non-fungible: research commits attribution; plan commits plan; only then redress dispatches (`feedback_triumvirate_discipline`).
- Build/test infrastructure improvements land FIRST in any tranche where dev iteration time is a bottleneck (`feedback_build_infra_first`).
- Generated code has per-tranche line-count budget; overflow blocks wave until O(N) generator regression traced (`feedback_generated_size_budget`).

---

## 9. PSI / DTA failure-mode anatomy (V9.5 excavation; canonical Lock 1 modes)

The V9.5 PSI excavation cohort (`V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §1) named 5 + 1 canonical failure modes. Each is load-bearing for evaluating SK-V4 / SK-V5 architectural proposals.

| Mode | Pathology | Measurement | SK-V4 disposition |
|---|---|---|---|
| 1.1 OpenFrame clone parallel substrate | Era IV/V `OpenFrame` deep-cloned in-flight `Vec<OpenFrame>` on speculative branch entry; CSS L4 carried 14 variants. | Samply measured **86.07% inclusive on `bbnf_value_twitter`** as `Vec<OpenFrame>::clone` from `JsonStructBuilder::checkpoint` (`restart/MIGRATION.md:347`). | VERIFIED-DIFFERENT in SK-V4: Lock 1 verbatim refutation ("no parallel offset stream"); rollback is a tape primitive, not a builder primitive. |
| 1.2 Type ambivalence | Three representations competing for "what the parse output is" (Tape ↔ OpenFrame ↔ direct-to-struct); CSS L4 carried 14 OpenFrame variants; AX.W1.A/B briefly landed direct-to-struct, deleted at W1r.0 (`3429aaba`: −6,128 LOC). | No code path could trust one representation without re-deriving from the others. Cost: indirect cloning, double materialisation, dead writes. | SAME-CLASS-DIFFERENT-INSTANCE in SK-V4: tape ∪ direct-to-struct as ONE substrate with five projections. `SinkOnly` is the only direct-only shape; the other four retain `(TapeId, cursor, event_kind_or_payload_class)` identity. Mitigated by single side-table field (`LayoutFacts.backend_shape`) with one 8-priority derivation algorithm. Falsifiable: if any shape collapses (e.g., EventTape → OffsetTape), AQ.5 recurs. |
| 1.3 Substrate-first / consumer-later (Era V signature) | Seven substrates shipped without runtime consumers (DTA, PSI, columnar tape, ShapeRef, PHF+SIMD classifiers, Bloom+GADT dedup, Shape emitter). | Era V close: **0/17 parse entries** exceeded the AU baseline; CSS / Sheets / BBNF landed at 3-7% of baseline; JSON twitter 486 MB/s (24.7% of AU). | Mitigated by `LESSONS-LEARNED.md:17-26` canonical rule: every substrate change must land with same-wave consumer or explicitly declared brittleness window. Gate G3: every wave exit cites a samply leaf at a consumer fn. |
| 1.4 Columnar SoA designed but never activated (AV.04) | `docs/tranches/AV/research/04-columnar-soa.md` (178 lines) specified kind-partitioned columnar store; Era IV Tranche Y activated 7 structural Vecs as per-kind columns. | **AY-I.W1 reverted to single `Vec<TapeRec>` + `sib_skip`** because 7-column AoS lost to cache-locality of a single AoS record. The AV.04 "kind-partitioned" variant never reached runtime. | VERIFIED-DIFFERENT in SK-V4: the three-array shape (offsets + flags + payload arena) is what simdjson, asmjson, yyjson, sonic-rs all retain — distinct from per-kind SoA. |
| 1.5 Per-grammar god-modules in generic crates | Lock 14 enumerated list: CSS L4 14-variant OpenFrame; BBNF aggregator `pub use bbnf::*`; Sheets arena fallbacks; per-grammar registry arms in `bbnf-ir`; `shape_dict_bbnf.rs`; `crates/core/src/css_types.rs`; per-grammar `runtime/<g>/` hand-written modules. | Enumerated 7 instances. | VERIFIED-DIFFERENT (provisional): `bbnf-simd` carries zero grammar names by trait surface (`SimdClassifier::classify_chunk`). Gate G4 (CI-gated): `rg JsonParser\|CssL4Parser\|... crates/{ir,parse,...}` returns zero. |
| 1.6 NEW: Interpreter-dispatch ceiling | DTA's `dispatch_one` carried 20-35% self-time across every grammar and every input size — "the canonical state-machine-interpreter ceiling." No AW-IV lever amortised it. | W2.1 prototype (`crates/bbnf-json-prototype/`) proved beat-sonic-rs achievable without PSI/DTA at all — `nm` for `dispatch_one` was empty in the bench binary. | Generalisation: any per-rule indirection through a runtime dispatch table (256-entry function-pointer table, `[DtaStateId; 256]` LUT, `match table.states[N]` over const table) pays a dispatch ceiling no SIMD/parallelism lever can amortise. Fix: emit dispatch INTO generated code as inlined `match`, not through runtime indirection. VERIFIED-DIFFERENT for `OffsetTape`; UNDETERMINED for `CollapsedStage` (the codegen-emitted-FSM concern). Gate G5: `cargo asm` shows no runtime-LUT dispatch in hot loop. |

The 5+1 modes are the canonical Lock 1 verbatim plus the excavation-surfaced sixth. The fix shape for mode 1.6 — emit dispatch into generated code, not through runtime indirection — is the controlling discipline for the `CollapsedStage` carve-out: NASM author per (grammar × ISA), never Rust codegen.

---

## 10. Spec amendment fold-back (SK-V3 -> SK-V4 -> SK-V5)

Surfaces patched during the iteration; each preserves the "no new BBNF directive / no new BIR variant" discipline.

| Surface | SK-V3 change | SK-V4 change | Source |
|---|---|---|---|
| `restart/skinny/BENCH.md` | Expanded corpus binding; native yyjson/asmjson sidecar planes added; workload split. | Corrected `alternate_capacity_plan` after grow-only Plan D. | SK-V3 §7; SK-V4 §8. |
| `restart/skinny/SUBSTRATE.md` | Lazy-offset triad pass preserved; typed event cursor over tape projection becomes canonical lowering target. | Structural projection IS tape (not a sidecar). | SK-V3 §7. |
| `restart/skinny/COMPILER.md` | Cost-model omission reclassified as MASKING for SOTA-BEAT; plan probes required. | Removed `CursorDispatch`/`set_len(0)` stale primitive wording. | SK-V3 §7; SK-V4 §8. |
| `restart/skinny/INDEX.md` | SIMD layer necessary but not sufficient; current expanded parse G rows + `N-direct / NoGo` visible. | Demoted two-pathology labels to diagnostics; corrected Plan D. | SK-V3 §7; SK-V4 §8. |
| `restart/skinny/WORKSPACE.md` | `bbnf-simd` is scanner + byte-primitive boundary; expanded gate blocks dispatch. | Redressed local LOC caps to 3,300 / 650 after direct-to-struct proof + `primitive-checkasm` became mandatory surfaces. | SK-V3 §7; `REDRESS.md:276-289`. |
| `restart/ARCHITECTURE.md` | `BackendShape` becomes materialization-plan enum: `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`. | Direct builders over event stream; added `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` diagnostic. | SK-V3 §7; SK-V4 §8. |
| `restart/MASTER-PLAN.md` | Tranche H becomes typed-event/SIMD/primitive close, host-aarch64 first and x86 strict plane second. | H.W5 primitive admission consumed by retained/direct shapes; per-grammar `CollapsedStage` separate. | SK-V3 §7; SK-V4 §8. |
| `restart/locks/14-LOCKS.md` | Lock 1 clarifies structural projection as tape; Lock 16 corpus wording uses expanded suite. | Locks 15 + 16 + Wave 1 strict-additive amendments (5-pack AVX-512 + 3-pack NEON). | SK-V3 §7; `HANDOFF.md:22`. |
| `restart/skinny/audit/SOTA-BEAT-DESIGN.md` | — | Replaced stale `StructuralIndex` and metadata collapsed-stage selection with five-shape cost-model-derived lowering. | SK-V4 §8. |
| `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` | Initial dispatch packet. | Demoted asmjson-beat projection to x86 successor-tranche; SK-V3 close remains M5 Max expanded gate. | SK-V4 §8. |
| `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md` | n/a (new) | Current receiver packet; closes generated `SinkOnly`, exact materializers, parse hot-hub attribution, same-wave primitive consumption, conditional x86 `CollapsedStage`. | `REDRESS.md:633-639`; `HANDOFF.md:24`. |

---

## 11. Falsifiability gates (G1-G10)

From `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md` §8. Each gate falsifies a specific PSI-recurrence shape.

| Gate | Falsifies | Measurement |
|---|---|---|
| G1: No OpenFrame in runtime | F1 (parallel substrate) recurring at cursor layer | `rg "OpenFrame\|Vec<OpenFrame>\|ParseStream" crates/runtime/src crates/codegen/src` returns zero. CI-gated. |
| G2: BackendShape side-table is single-source | F2 (type ambivalence) via multiple decision surfaces | `rg "EmissionTier\|BackendShape\|emission_strategy" crates/ir/` returns exactly one decision surface. |
| G3: Wave consumer cite | F3 (substrate-first/consumer-later) | Every wave exit cites samply leaf at consumer fn with ≥X% self-time. Wave blocks if no cite. |
| G4: No grammar names in generic crates | F5 (per-grammar god-modules) | Lock 14 grep returns zero. CI-gated. |
| G5: No runtime dispatch tables in hot path | F6 (interpreter-dispatch ceiling) | `cargo asm` shows no `match table.states[N]` runtime-LUT dispatch in hot loop. |
| G6: BackendShape firing matrix shows ≥2 shapes across 9-grammar matrix | F2 taxonomy redundancy (AQ.5 precedent) | `derive_backend_shape` audit emitted per grammar. |
| G7: Phase 4 (`CollapsedStage`) has its own differential parity harness BEFORE dispatch | Codegen-emitted-FSM concern | `tests/fsm_codegen_parity.rs` exists, runs cost-model-derived FSM bytecode vs hand-written reference, 0 divergences. |
| G8: Phase 4 admission produces ≥1 non-JSON CollapsedStage firing OR records "JSON-only" as known scope | Lock 14 god-module at cost-model level | `derive_backend_shape` per-grammar verdict table. |
| G9: Expanded SOTA gate on M5 Max | Substrate viability for SOTA-BEAT claim | 0 G-rows on 17-corpus expanded gate; `random` and `unicode_escapes` no longer dominant samply leaf at `parse_value_at`. |
| G10: yyjson/simdjson/asmjson sidecar comparators reported with strictness plane | Comparator-basis honesty (`feedback_no_warm_benches`) | RESULTS.md table has columns for strictness, mode, host CPU, warm-vs-cold per-parse. |

G1, G2, G4, G5 are CI-gated (block merge). G3, G6, G7, G8, G9, G10 are tranche exit gates.

---

## 12. Sidecar comparator strictness plane

The strictness / mode / host / warm-cold reading per comparator (SK-V4 §5):

| Comparator | Strictness | Output mode | Host posture | Warm/cold |
|---|---|---|---|---|
| skinny v3 (Track 1) | Strict RFC 8259 | Lazy `OffsetTape` + view | M5 Max arm64 | Cold per-parse |
| skinny v3 (Track 2) | Strict RFC 8259 | Lazy `OffsetTape` + view | M5 Max arm64 | Cold per-parse |
| sonic-rs (Value) | Strict | Value DOM | M5 Max arm64 | Cold per-parse |
| sonic-rs (LazyValue) | Strict | Lazy raw-slice | M5 Max arm64 | Cold per-parse |
| simdjson C++ (DOM) | Strict | DOM | M5 Max arm64 | Cold per-parse |
| simdjson C++ (On Demand) | Strict | Forward-only cursor | M5 Max arm64 | Cold per-parse |
| yyjson (inlined) | Strict (RFC 8259 + accurate numbers) | DOM | M5 Max arm64 | Cold per-parse |
| asmjson (M5 Max SWAR) | **Permissive** (accepts ctrl as whitespace; does not fully scan strings for unescaped controls) | SAX sink / flat DOM | M5 Max arm64 SWAR | Cold per-parse |
| asmjson (Zen 4 AVX-512) | **Permissive** | DOM | Zen 4 AVX-512 (published anchor only) | Published |

Strict vs permissive comparison must be split in `RESULTS.md`. asmjson's 10.93 GiB/s headline is a permissive measurement; the strict equivalent has not been published.

---

## 13. Masking probe ledger (post-SK-V4 RESULTS.md reading)

From `RESULTS.md:45-150`. Five probe categories per corpus; reading:

| Probe | Status | Reading |
|---|---|---|
| `host_call_dispatch_overhead` | PASS ≤ 50 ns | 0.59-0.76 ns/call across all 17 corpora. Dispatch is not the cost. |
| `host_call_eager_decode` | MASKING > 1.10× T1 (17/17) | 17.9% - 46.8% of Track 1 throughput across all corpora. Parse-time string decode is too expensive to hide behind host-fn-free cut. V1 JSON must keep decode lazy. |
| `alternate_scalar_plan` | reported (informational only) | 25.5% - 64.8% of Track 1 throughput; no decision impact. |
| `alternate_dispatch_table_plan` | INVALID (duplicate-probe disabled; real function-pointer table regressed) | Across all 17 corpora. |
| `alternate_pext_mask_plan` | missing | Not implemented. |
| `cold_first_parse` | PASS ≤ 2.00× T1 | 66.8% - 104.5% of Track 1; cold first parse is within 2× of warm. |

---

## 14. Tally

- Validated: 12
- Invalidated: 18
- Demoted / narrowed: 3
- Still open: 14
- Rejected routes: 20
- PSI failure modes (5 + 1 canonical): 6
- Falsifiability gates (G1-G10): 10
- Spec surfaces patched in SK-V3 → SK-V4 fold: 12
- BackendShape per-grammar matrix entries: 13
- Total ledgered items: 108

---

## 15. Implementation debt snapshot (SK-V4 binding next-work)

From SK-V4 §6 and `reassay-skv4-2026-05-13/PROFILE-REPORT.md` §"Binding Next Work":

1. **Generated SinkOnly migration**: Track 1 direct must move out of `bbnf-bench` into generated `runtime`/`codegen` `SinkOnly`. Current Track 1 direct still calls a bench-private parser. SK-V4 packet Wave 1.
2. **Exact materializers**: direct rows must use exact generated string/Unicode/number materializers. Eisel-Lemire mantissa multiplication for `parse_number`; `\uXXXX` TBL decode wired into `unescape_json_string`. SK-V4 packet Wave 2.
3. **PC-level attribution for parse hot hub**: `parse_value_at` shows 99.5-99.7% symbol-level self-time. No-inline diagnostic build or address-map report required before prescribing another kernel. A new primitive that is checkasm-green but leaves `parse_value_at` at ~100% self-time has not proven anything about the current parse-G rows. SK-V4 packet Wave 3.
4. **EventCursor as lowering boundary**: route `OffsetTape` hot alts through `EventCursor` only where the profile proves source-byte dispatch is the current cost. The full prototype (mask + LUT + `ptr`/`end` sentinel + `#[inline(never)]` cold-path) is in flight; scoped-down version refuted.
5. **Same-wave NEON primitive consumption**: land only same-wave consumed NEON primitives (byte class, next-set-bit, hex decode, exact digit blocks). No primitive without a wave-bound consumer.
6. **x86 CollapsedStage successor**: stay separate until NASM author, silicon, and checkasm are all present. `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` is the correct fallback. Per-grammar × ISA authoring waves are grammar-arrival-driven, not numbered into the H letter tranche.

The dispatch order is **SK-V4 IMPLEMENTATION-PACKET Waves 0-5 in sequence**, as named at `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md`. SK-V3 packet remains historical context; it is no longer the receiver packet.

---

## 16. Verification rituals (HANDOFF §7)

Before any wave dispatch:

```
ls skinny/profile/{native-sidecars,wave2-asm,wave2-pmu,wave2-capacity,wave2-prototype}/PROFILE-REPORT.md
head -30 restart/skinny/audit/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md
head -30 restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md
grep -c "^15\.\|^16\." restart/locks/14-LOCKS.md   # >= 2
grep -E "LD4-interleaved|BCAX|EOR3|svmatch_u8|kreg-facts|VPCLMULQDQ|vpmadd52|vpdpbusd|vpshufbitqmb" restart/locks/14-LOCKS.md | wc -l   # >= 5
grep -E "BBNF-BACKEND-SHAPE-INCONSISTENT|BBNF-FORCE-INLINE-MISSED|BBNF-ICACHE-BUDGET-EXCEEDED|BBNF-UTF8-INVALID-AT-PARSE|BBNF-UNICODE-NONCHAR-CODEPOINT" restart/ARCHITECTURE.md | wc -l   # 5
grep -E "EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage" restart/ARCHITECTURE.md | wc -l   # >= 5
rg -n 'tape_mode|lazy-mode|dual-mode' restart/locks/14-LOCKS.md   # 0 matches (Lock 1 verbatim preserved)
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity   # zero divergences
cargo run -p xtask --release -- bench-json --capacity-plan D --corpus update-center
cargo build --release -v 2>&1 | grep -E -- '-C lto=(fat|true)|-C codegen-units=1' | wc -l   # >= 1 per member
```

Per-corpus reproduction:

```
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- gate-json
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o profile/reprofile-sk-v3/random.event_cursor.profile.json.gz \
  -- ./target/release/profile-lazy 50000 test_data/random.json
```

---

## 17. Lock provenance summary

| Lock | Title | Status |
|---|---|---|
| Lock 1 | Tape ∪ direct-to-struct as ONE substrate; no parallel substrates; no OpenFrame ladders | Verbatim; lazy-tape amendment applied (`HANDOFF.md:64`) |
| Lock 5 | V1 ships `RustBackend: Backend` only; WASM defers post-V1 as `WasmBackend: Backend` | Active |
| Lock 7 / Lock 11 | `path-ts` defers post-V1 alongside V2 `TsBackend: Backend` impl | Active |
| Lock 8 | V1 SOTA close gates measure the Rust line only; WASM SOTA defers post-V1 | Active |
| Lock 10 | Pratt and SIMD are auto-detected; cost model selects per-grammar `backend_shape` | Active; 8-step `derive_backend_shape` algorithm |
| Lock 14 | Full grammar generalisation; zero overfitting; no grammar names in generic crates | Active; CI-gated (G4) |
| Lock 15 | Build-profile discipline (`lto=fat codegen-units=1 panic="abort" debug=true`); i-cache budget ≈ 20 KiB | Landed 2026-05-12; yyjson evidence + skinny PMU confirm |
| Lock 16 | SIMD/ASM admissibility allowlist; abstract primitive lifts from dav1d/ffmpeg; Wave 1 5-pack AVX-512 + 3-pack NEON strict additions | Landed 2026-05-12; extended same day |

---

## 18. Most surprising finding from the consolidation

The most surprising finding is that **the asmjson 10.93 GiB/s SOTA-BEAT anchor is not AVX-512 esoterica at all** — disassembly of asmjson's tight loop (Wave 1 Agent 1; `WAVE-1-2-COHORT-DIGEST.md:31-38`) shows only 6× `vpcmpeqb`, 10× `kmovq`, 2× `vpcmpub`, 6× `korq`, 2× `vmovdqu8`, 18× `tzcnt`. **Zero exotic encodings.** The 5× margin over yyjson is sourced from a 9-state DPDA with PC-as-state direct threading via `r10`, `tzcnt`-driven seek, and msac-style EOB padding — not 512-bit width. asmjson's own SWAR fallback already lands 7 GiB/s on the same host.

This inverts the original SOTA-BEAT framing: the architectural lever is the fused 9-state DPDA + direct-threaded dispatch + per-state classifier masks, not the instruction width. AVX-512 esoterica (k-mask arithmetic family, VPCLMULQDQ-512, AVX-IFMA, VNNI, BITALG, GFNI) is a strict additive lift *on top of* asmjson's architecture — not a substitute for it.

The second-order surprise: this is exactly the AW-V recurrence shape the V9.5 PSI excavation warned about. "Codegen-emit FSM derived from any Grammar IR" is structurally the same problem as AW-V's "auto-derive the sonic-rs-class inner loop from any BBNF grammar" thesis, which demonstrated once on JSON at W3 and was lost by W6. The DTA's `dispatch_one` carried 20-35% self-time as "the canonical state-machine-interpreter ceiling" — the same shape as the `[DtaStateId; 256]` LUT, the same shape as a 256-entry function-pointer table. **The fix is structural: emit dispatch into generated code as inlined `match` (LLVM jump-table); never through runtime indirection. `CollapsedStage` admits only NASM authored per (grammar × ISA), never Rust codegen.**

A third-order surprise lurks in the workload split: the historical triad (twitter / citm / canada) was insufficient evidence because it averaged over corpus shapes that genuinely have *different* hot leaves. simdjson stage1 vs stage2 inverts on `unicode_escapes` (8.8% / 60.7% / 30.5%); skinny v3 already wins on `citm`/`canada`/`mesh`/`unicode_mixed` against simdjson C++; the remaining gaps concentrate in small-object-heavy ASCII (apache_builds / github_events / update_center) where simdjson stage1 amortisation has the most to give, plus string-decode-heavy Unicode (random / unicode_escapes / y_string_unicode) where stage2 is the bottleneck. **No single architectural lever closes all five red rows.** Class A NEON kernel closes github_events/update-center/random (object-heavy); Class B closes unicode_escapes/y_string_unicode (escape-heavy); generated `SinkOnly` with exact materializers closes the direct red rows. Three separate prescriptions, one cost-model.

A fourth surprise — the simdjson stage1 inversion on escape-heavy corpora — is the same evidence that ratifies skinny's lazy-decode-on-view stance: when escapes hide inside strings, stage1's SIMD classifier has nothing to do; the work moves into stage2's `parse_string` + escape decoder. simdjson is **architecturally identical** to skinny on those rows; the gap is local primitive quality, not stage layout. The smallest absolute gap on any NO-GO corpus (`unicode_escapes` 587 vs simdjson 672 MiB/s, 85 MiB/s absolute) is therefore the highest-yield target: a single inner-loop tightening of the surrogate-decode path.

---

## 19. Closing posture

The skinny JSON triad is green; the current expanded parse plane has four G rows (`twitter`, `random`, `unicode_mixed`, `unicode_basic`), and the direct workload is `N-direct / NoGo` with 6 of 17 rows passing and 11 failing. The next close is generated `SinkOnly` typed emission plus exact float/string/Unicode materialization, alongside event-cursor consumption for the four parse rows. Plan D and the admitted NEON primitives remain useful, but they do not by themselves close the gate.

Skinny v3 ALREADY wins on 4 of 17 corpora vs simdjson C++ / yyjson on M5 Max (citm, canada, mesh, unicode_mixed) — measured, not projected. The AVX-512 esoterica stack landed at Lock 16 as strict additions on top of the asmjson architecture for the >SOTA path on x86_64. All citation-anchored. All conditional on hardware. None violates Lock 14.

The 16 locks govern. The 5-shape `BackendShape` generalises across JSON / CSS L4 / BBNF-self / Sheets without per-grammar code in any generic crate. V1 planning carries the triad pass as substrate evidence, the four-row parse G set plus `N-direct / NoGo` as the current implementation block, and the per-grammar `BackendShape` matrix as the lowerer-template contract.

Total ledgered items: 108 across validated (12) + invalidated (18) + demoted (3) + still-open (14) + rejected-routes (20) + PSI failure modes (6) + falsifiability gates (10) + spec surfaces patched (12) + per-grammar BackendShape matrix entries (13). This ledger consolidates V9.2 lazy-tape, V9.3/V9.4 grand-synthesis (asmjson + dav1d), V9.5 PSI excavation, SK-V3 (5-shape + checkasm + Class A/B), and SK-V4 (N-direct honesty + asmjson demotion + generated SinkOnly mandate) into a single citation-grounded document.
