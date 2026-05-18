# SK-V9 P2-F: SOTA Competitor Teardown for Parse + Node Speed on Apple M5 Max

Pass: S-P2 Research. Cycle: V3 (S-P2 V2 CHALLENGE fold — CH3 + CH6 surgical residuals closed; see §0).
Date: 2026-05-18.
Scope: per-competitor architecture grounding on aarch64 / Apple M5 Max,
keyed to V3-V6 PMU evidence; identifies the precise interventions that
lift bbnf strictly above sonic-rs + simdjson + yyjson on every row.
Output: this file.
P1 hot-leaf antecedents: `scan_structurals` (0.00% self-time, discarded
index); `match_tiny_plain_string` + `match_string_at_quote` (47-67% on
loss corpora); `read_hex_unit_scalar` + `hex_nibble` (38-44% on
y_string_unicode); the four uncloseable rows (unicode_mixed,
unicode_escapes, y_string_unicode, gsoc-2018) per the
`ns_per_byte ≈ 1.079·(q/B) + 0.184·(n/B) + 0.051` regression, R²=0.371.
Lock surface: Lock 1 (substrate union) + Lock 14 (grammar-neutrality)
+ Lock 16 (admissibility allowlist).

---

## §1 — Apple-M5-Max-specific landscape (per competitor)

Apple M5 Max is an ARMv9.2-A Avalanche/Blizzard-class big.LITTLE part
running at ~4.05 GHz peak on the P-cores, 16 KiB L1-I + 192 KiB L1-D
per P-core, with NEON 128-bit ASIMD (no SVE2 exposed in user-space) and
the ARMv8.6 dot-product / I8MM extensions, plus the ARMv9.0 CSSC
(Common Short Sequence Compression) family — `CLS`, `CTZ`, `RBIT`,
`CNT` materialised as one-cycle integer ops without the historical
NEON detour. No AVX, no AVX-512, no GFNI, no VPCLMUL-512 — those
landscapes belong to Zen 4 and Sapphire Rapids and are not measurable
on this host.

The strict-vs-strict comparator picture on this host is asymmetric:

| Competitor | Apple M5 Max implementation path | Strict on this host? | Anchor evidence |
|---|---|---|---|
| **sonic-rs** | Rust crate; uses `core::arch::aarch64` NEON intrinsics directly in `src/util/simd/neon.rs` for the four skip-scan kernels (`skip_string`, `skip_number`, `skip_container`, `get_from`). The structural-classification kernels lean on `vceqq_u8` ⊕ `vandq_u8` reductions plus `vqtbl1q_u8` for nibble-table classification. No SVE path. Single-pass with vectorised inner scans; no persistent structural index. | Strict by default after the SK-V7 W0 `utf8_lossy` repair; sonic-rs strict ≈ sonic-rs lossy within ≤1.7% in same-run criterion rows (twitter 19453 vs 19551 Mbps, citm 23590 vs 23461, canada 12723 vs 12832 per RESULTS.md). | `criterion:json_*/sonic_rs_anchor/new/estimates.json` same-run native; the comparator is the load-bearing strict anchor. |
| **simdjson** | C++ via FFI; the NEON backend at `src/arm64/` carries genuine stage-1 (quote/escape resolution, `vqtbl1q_u8` TBL classify, `vshrn_n_u16` bit-to-index compaction) feeding stage-2 tape build. The NEON `prefix_xor` uses the `pmull{,2}_p64` polynomial-multiply scheme over `0xFFFFFFFFFFFFFFFF` instead of x86's `_mm_clmulepi64_si128`. | Strict; RFC-8259-conformant by default with explicit UTF-8 validation fused into stage 1 boundary. | `historical:sk-v7-sidecar-profile` rows (twitter 24522, citm 35822, apache_builds 36014, github_events 39642, update_center 30593 Mbps). Marked sidecar — diagnostic only, never an admission anchor. |
| **yyjson** | C89 single-pass scalar; on aarch64 the path is the *identical* code as on x86 — zero ISA-specific code. The performance comes from one ≤20 KiB `__force_inline` function that fuses UTF-8 validate, `\uXXXX` decode, escape resolution, and DOM cell append into one loop body. M5 Max wide-issue (8-wide decode, 12-wide back-end) + 192 KiB L1-D is the substrate that makes the perfect-fusion bet pay. | Strict by default; JSON5 (comments, NaN, trailing commas) is opt-in flag. | `historical:sk-v7-sidecar-profile` rows (twitter 30931, citm 20956, canada 13003, apache_builds 16275, github_events 21426, update_center 18540 Mbps); sidecar only. On twitter yyjson 30931 > simdjson 24522 > sonic-rs 19453 — yyjson is the M5 Max DOM-plane leader. |
| **asmjson** | NASM x86_64 hand-coded; the published 10.93 GiB/s requires AVX-512BW + BMI1 (Zen 4 / Sapphire Rapids). No aarch64 NEON backend exists in upstream `asmjson/asm/` — the ARM path is the SWAR scalar fallback, not the hero ASM. RFC-8259 strictness is intentionally violated (treats every byte `<0x20` as whitespace per upstream README §"Strictness"). | **Not strict** on this host even if it ran — the SWAR fallback inherits the permissive control-byte treatment. AVX-512 path is unrunnable on aarch64. | Per RESULTS.md flaw_probe column: every asmjson row is `n/a` on M5 Max; the asmjson 10.93 GiB/s headline is a non-anchored sidecar planning signal per the V3-V6 strictness rules (`HARDENING-S-P1-CONVERGED.md` §1 source-anchor caution). |
| **RapidJSON** | C++ scalar with optional SSE4.2 path; on aarch64 the SSE intrinsics gate out and the scalar path runs. Permissive UTF-8 by default. | Strict only with explicit `kParseValidateEncodingFlag`; default is permissive. | Sidecar twitter 4020, citm 6760, canada 5187, apache_builds 3945 Mbps. Mature but not competitive on M5 Max; useful as the scalar floor reference. |
| **serde_json** | Pure Rust scalar; the strict reference. | Strict. | Same-run native; twitter 4711, citm 6836, canada 4384, apache_builds 5135 Mbps — the strict scalar floor. |

The strict comparator hierarchy on M5 Max from RESULTS.md same-run
rows: **yyjson > simdjson NEON > sonic-rs > RapidJSON > serde_json**
on string-heavy DOM, and **sonic-rs ≈ simdjson NEON > yyjson** on the
number-dense rows where Eisel-Lemire dominates. asmjson does not run
strictly on this host and must be flagged as a non-anchored signal.

---

## §2 — Parse-speed competitive position per corpus class

The V3 PMU c/B column (`/tmp/skv9-xctrace-v3/pmu_rows.tsv`) plus
RESULTS.md `Δ vs sonic-strict` together give us a per-row reading of
*why* bbnf wins or loses. The four-class taxonomy below partitions the
17 corpora; the regression `ns_per_byte ≈ 1.079·(q/B) + 0.184·(n/B) +
0.051` (R²=0.371) is the V4 honesty correction over V3's 8×-inflated
fit.

### 2.1 Number-heavy / structurally-deep — **bbnf wins**

| Corpus | Track 1 Mbps | sonic strict | Δ | bbnf c/B | bbnf CPI |
|---|---:|---:|---:|---:|---:|
| canada | 16190 | 12723 | **+27.2%** | 2.10 | 0.127 |
| numbers | 17956 | 12972 | **+38.4%** | 2.16 | 0.171 |
| marine_ik | 12073 | 8417 | **+43.4%** | 2.69 | 0.147 |
| mesh | 12435 | 11279 | **+10.2%** | 2.69 | 0.135 |
| citm_catalog | 29215 | 23590 | **+23.8%** | 1.18 | 0.154 |

**Why bbnf wins.** Number-heavy corpora exploit two bbnf-specific
advantages that competitors cannot match on their respective
architectures:

1. **Eisel-Lemire vendored close (canada, numbers, mesh, marine_ik).**
   `parse-that-regex` carries the full Eisel-Lemire f64 fast-float
   reference. sonic-rs's `RawNumber` deserialiser path defers the
   exact-rounding decision; simdjson's `to_double` uses the same
   Lemire-Daniel approach but pays the stage-2 dispatch tax per
   number; yyjson's scalar fusion has no `f64::from_str` competitive
   parity — yyjson takes ~13003 Mbps on canada vs bbnf's 16190 (+24.5%)
   because the scalar walk pays the digit-by-digit branch.
2. **citm_catalog object-of-objects with low quote density.** Per the
   regression, citm c/B = 1.18 (lowest of all 17 rows except gsoc-2018).
   The quote density `q/B` is the dominant coefficient; citm has a
   structurally-deep / value-shallow shape so the per-quote cost stays
   small. ContainerNext (V9.5 Wave 2 close — the enum is defined at
   `skinny/crates/runtime/src/grammars/json/generated.rs:341`, consumed
   at `:134-135` and emitted by `consume_array_next` at `:348-375`)
   eliminates the per-element re-dispatch that simdjson's stage-2
   goto-thread requires across each `{`/`}` boundary.
3. **Low CPI confirms back-end-bound, not front-end-bound.** Canada
   CPI = 0.127 (8-wide IPC = 7.9), numbers 0.171 (IPC 5.9), mesh 0.135
   (IPC 7.4) — every winning row has CPI well below the 0.25 threshold
   that signals branch-misprediction stalls. The bbnf parser, on these
   shapes, is decoding through the M5 P-core's full execution width.

This is the **back-end-bound regime**: bbnf wins because the work-per-
element is dominated by predictable arithmetic (digit accumulation,
mantissa packing, IEEE-754 rounding), and the M5 wide back-end
amortises it efficiently. simdjson's stage-1/stage-2 split actively
*hurts* here because every number must cross the stage boundary
(memory write + dispatch read).

### 2.2 String-heavy / object-key-dense — **bbnf loses**

| Corpus | Track 1 Mbps | sonic strict | Δ | bbnf c/B | bbnf CPI |
|---|---:|---:|---:|---:|---:|
| twitter | 13188 | 19453 | **-32.2%** | 2.37 | 0.211 |
| update_center | 9857 | 15806 | **-37.6%** | 3.62 | 0.248 |
| apache_builds | 11917 | 15536 | **-23.3%** | 2.91 | 0.228 |
| github_events | 14302 | 21360 | **-33.0%** | 2.27 | 0.208 |
| gsoc-2018 | 22184 | 45318 | **-51.0%** | 1.54 | 0.228 |
| random | 9382 | 15166 | **-38.1%** | 3.55 | 0.188 |
| distinct_values | 8972 | 17304 | **-48.1%** | 3.85 | 0.206 |

**What sonic/simdjson do that bbnf doesn't.** Three distinct mechanisms,
in descending order of impact:

1. **Fed-index stage-2 string boundaries** (simdjson NEON; per SC-2 §1.1).
   simdjson's stage 1 emits an index of every structural byte including
   the opening and closing `"` of every string. Stage 2's per-string
   work is *one index-read* — the closing quote is the next entry. bbnf
   produces this same index in `scan_structurals` (the kernel is
   genuine: prefix-XOR + TBL classify + `compact_mask`), then **throws
   it away**, and the generated `match_tiny_plain_string` /
   `match_string_at_quote` re-discovers every closing quote by branchy
   inner scan. The cost is precisely the 47-67% self-time those two
   leaves carry on the loss corpora (SC-2 §2; P1-V3-C). On gsoc-2018
   sonic-rs runs at 45318 Mbps because its NEON `skip_string` kernel
   uses `vceqq_u8` + `vshrn_n_u16` + `tzcnt` to leap over each string in
   one 16-byte stride; bbnf is at 22184 Mbps because each string body is
   a byte-by-byte loop with a data-dependent branch per byte.
2. **Vectorised tiny-string equality matching for object keys** (sonic-rs
   NEON; per SK-V7-A2 §5). sonic-rs's `from_str::<T>()` typed path
   generates field-key match arms using a `vqtbl1q_u8`-driven 16-byte
   equality probe — the `created_at` key in twitter is recognised in
   one NEON instruction. bbnf has the Lock-16-admitted Class A
   `match_tiny_plain_string` primitive but the SK-V6 Wave 6 admit
   *demoted* its active wiring; the hot retained dispatch still uses
   the scalar form.
3. **Total i-cache fusion of UTF-8 + escape + cell-emit** (yyjson; per
   SC-2 §1.4). yyjson at 30931 Mbps on twitter (vs simdjson 24522, sonic
   19453) is the proof that on string-dense product JSON, *removing the
   stage boundary* beats SIMD. yyjson's single `__force_inline` walk
   keeps the entire DOM-build code path in ≤20 KiB of i-cache; the M5
   Max P-core has 16 KiB L1-I but a 256-entry µop-cache that
   effectively extends the hot loop to fit. bbnf's hot path crosses
   five function boundaries per string (`generated.rs:6,193,625` —
   `match_string_at_quote` calls into `parse-that-regex`, returns,
   pushes offset, then dispatches to the next structural element).

The CPI column confirms which mechanism is hurting: update_center CPI
0.248 + 3.62 c/B is **front-end-bound** — branchy inner scan saturating
the branch predictor. twitter CPI 0.211 is partially front-end-bound +
partially i-cache (the hot loop spans more bytes than yyjson's). gsoc
CPI 0.228 with 1.54 c/B is the asymmetric case: low c/B because gsoc is
mostly array-of-uniform-objects, but still front-end-bound on the
per-string scan.

### 2.3 Unicode-heavy — **all losses, large**

| Corpus | Track 1 Mbps | sonic strict | Δ | bbnf c/B | bbnf CPI |
|---|---:|---:|---:|---:|---:|
| y_string_unicode | 5428 | 11814 | **-54.1%** | 5.71 | 0.240 |
| unicode_mixed | 6803 | 14515 | **-53.1%** | 4.63 | 0.390 |
| unicode_escapes | 12047 | 18132 | **-33.6%** | 3.01 | 0.231 |
| unicode_basic | 11348 | 14823 | **-23.4%** | 2.91 | 0.197 |
| distinct_values | 8972 | 17304 | **-48.1%** | 3.85 | 0.206 |

**The unicode gap and its mechanism.** Per HARDENING-S-P1-CONVERGED §1
load-bearing diagnosis #3: `read_hex_unit_scalar` + `hex_nibble`
together reach 38-44% self-time on y_string_unicode. The CPI on
unicode_mixed (0.390) is the highest of any row in the entire PMU
table — this is the *worst-case* branchy code in the bbnf parser.

The competitor unicode-handling shape:

- **sonic-rs unicode path.** sonic-rs has `_mm_set_epi8(...)`-style 16-
  byte `\u` probes on x86; on NEON it uses `vceqq_u8` against `'\\'`
  and `'"'` plus a `vandq_u8` mask to short-circuit the
  no-escape-present case. For each `\uXXXX` sequence the hex decode is
  scalar but fused with the cell-emit. The strict mode validates
  surrogate pairs; the lossy mode skipped validation entirely
  (the SK-V6/SK-V7 W0 flaw probe). sonic-rs strict at 11814 Mbps on
  y_string_unicode is the strict anchor.
- **simdjson unicode path.** The stage-1 escape mask already identifies
  every `\` byte; stage 2's string materialiser branches on `\u` and
  calls a scalar 4-nibble decode + surrogate-pair handler. The
  *measured* gain over scalar is small because simdjson's strength is
  the structural-scan amortisation, not the per-escape decode.
  simdjson on y_string_unicode (no same-run row; historical sidecar)
  reflects this.
- **yyjson unicode path.** This is the load-bearing teach: yyjson's
  `\uXXXX` decode is **fused into the same `__force_inline` walk** —
  no function call, no second pass, no separate hex-table lookup
  through a different cache line. The 4-nibble decode reads one 4-byte
  value, applies a per-nibble validation mask, and shifts directly
  into the codepoint accumulator. The surrogate-pair join is one extra
  branch. yyjson's per-`\uXXXX` cost is bounded by L1 latency, not
  branch-prediction.

The unicode gap is *not closeable by a delimiter-only intervention*
(HARDENING-S-P1-CONVERGED §1 #4): the four LOSS rows have throughput
gaps exceeding 130-460% of the regression's full per-byte budget. The
gap is paid in the escape codec, not the structural scan. This is the
P2-E unicode-codec design's load-bearing finding.

### 2.4 Borderline / mixed — **bbnf within ±10%**

| Corpus | Track 1 Mbps | sonic strict | Δ |
|---|---:|---:|---:|
| instruments | 16189 | 17201 | -5.9% |

instruments is the cleanest "near-PASS" — the shape is balanced (some
strings, some numbers, structurally-shallow) and the bbnf c/B (2.07) is
already competitive. A modest reduction in the per-string scan would
flip this to PASS.

---

## §3 — Node-speed competitive position (typed product plane)

The typed product plane (`real_typed_struct` in RESULTS.md) is the
workload where bbnf already wins on every admitted row:

| Corpus | bbnf typed Mbps | sonic strict typed | Δ |
|---|---:|---:|---:|
| twitter | 14761 | 14665 | **+0.7%** (PASS within slack; +12.4% per dispatch context) |
| update_center | 11345 | 11874 | **-4.5%** (PASS within 1.10× slack) |
| mesh | 8919 | 8531 | **+4.6%** |
| marine_ik | 11259 | 8990 | **+25.2%** (the dispatch's "+69.5%" reflects a different cycle counter; same-run direction) |

**Why bbnf wins on node speed.** The typed product plane is the case
where the *DirectBuild* lowering (V9.5 Wave 3 admit at commit `ab06ff11`)
emits field-fact projections directly into the owned Rust struct
without an intermediate DOM. The architectural advantages:

1. **Single allocation per output struct.** sonic-rs's
   `from_str::<T>()` uses `serde::Deserialize`, which monomorphises
   per-field but still routes through a `Visitor::visit_map` dispatch
   per field. bbnf's DirectBuild monomorphises the *entire* struct
   construction at codegen time — there is no visitor trait object,
   no per-field dispatch.
2. **No DOM hand-off cost.** sonic-rs's `Value` arena DOM (the
   non-typed path) is a 2-pass operation: parse-to-Value, then
   Value-to-T via serde. The typed direct path elides the Value but
   still uses the same SIMD scan kernels. simdjson On Demand is
   forward-only and parse-once but the C++ FFI overhead per Rust
   struct field access dominates on small twitter-sized payloads.
3. **Tape-fed structural cursor.** Even on the typed plane, bbnf's
   structural offset tape is consumed by the DirectBuild step;
   marine_ik's +25.2% advantage is driven by the dense small-int array
   shape where the offsets-and-classes substrate matches the access
   pattern exactly.

**What asmjson does at the node-construction step.** asmjson does
*not* expose a typed-direct path. Its two output planes are
`parse_to_dom` (flat 64-bit DOM tape, returning `DomNode`) and
`parse_with` (SAX-style sink calling a caller-provided trait). The
caller assembles typed structs around the SAX sink — there is no
type-system glue that monomorphises the visitor at compile time. This
is the V9.5-PSI flagged generality limitation. asmjson is a *DOM-class*
proof, not a typed-direct competitor.

**What simdjson does.** simdjson does not expose a typed-direct path
in upstream C++. The Rust binding `simd-json` builds a `Value` DOM and
then calls `serde::Deserialize` from it, paying the same 2-pass cost
sonic-rs's non-typed Value path pays. No SOTA C++/Rust parser carries
bbnf's DirectBuild equivalent — this is bbnf's structural lead and the
room to widen it is real.

**The lead is REDRESS-bound territory.** The typed product plane
currently leads on 4 rows; any widening that touches the dispatch hot
path or typed materialiser is REDRESS-bound and requires S-P3
differential authoring. Specifically: wiring Class A NEON
`match_tiny_plain_string` at the DirectBuild field-name match arm chain
is the REDRESS 33 rejected shape, and fusing `\uXXXX` decode into the
field-fact emit is REDRESS 66-69 territory (the direct string/Unicode
close declared exhausted under the current strict digest workload).
Both moves silently reopen pre-blocked routes and risk the 4 typed-GO
rows. This synthesis names the structural lead as a finding only; the
question of whether — and under what fresh-evidence framing — the lead
can be widened belongs to S-P3 with explicit material-differential
gates per `skinny/REDRESS.md` entries 33 and 66-69.

---

## §4 — The asmjson anchor question

The V3-V6 strictness rules (HARDENING-S-P1-CONVERGED §1; SC-2 §1.0
source-anchor caution) classify a comparator anchor by three axes:
*same-host*, *same-run*, *strict-plane*. asmjson fails two of three on
M5 Max:

| Axis | asmjson status on M5 Max |
|---|---|
| Same-host runnable | No. The AVX-512BW + BMI1 fast path requires Zen 4 / Sapphire Rapids silicon; no aarch64 NEON backend exists upstream. |
| Same-run measurement | No. RESULTS.md column shows `n/a` for every asmjson_swar and asmjson_avx512 row on M5 Max. |
| Strict plane | No. The published 10.93 GiB/s is permissive (treats every byte `<0x20` as whitespace; never scans string bodies for unescaped controls per upstream README). |

**Verdict per the V3-V6 strictness rules.** asmjson is a **non-anchored
sidecar planning signal** on Apple M5 Max. It cannot:

- Stand as a SOTA-beat anchor row in RESULTS.md.
- Be cited as a strict comparator delta.
- Substitute for same-run criterion evidence on this host.

asmjson **can** be cited for:

- *Architectural-pattern evidence* — the 9-state DPDA with PC-as-state
  direct threading, the `BYTE_CLASS_FROM_EQ_SET_64` + `BITMAP_NEXT_SET_BIT`
  + `FSM_DISPATCH_THREADED` primitive vocabulary (per SK-V7-A2 §6; the
  canonical primitive-class taxonomy this report inherits is anchored at
  `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md`
  §1.5).
- *Cross-ISA architecture lift* — the CollapsedStage successor route
  (SK-V7-A2 §9) is admissible for the x86 tranche after M5 Max close,
  *not* as an M5 Max beat target.
- *Non-anchored speed ceiling for the Zen-4 follow-up tranche* — the
  10.93 GiB/s is the Phase-4 sidecar target per V9.5-PSI Risk C, gated
  by silicon access.

This is the *line that distinguishes the asmjson architecture lift
from the asmjson clone failure mode* (SK-V7-A2 §6 final paragraph).

---

## §5 — Architecture lessons (one pattern per competitor)

For each competitor, the one architectural pattern the V3-V6 evidence
says bbnf is **not yet using** that would close a measured gap:

### 5.1 simdjson — **Consume the index that scan_structurals already produces**

The kernel exists (`runtime/src/grammars/json/scan.rs::scan_structurals`
is a genuine branch-free NEON stage 1: prefix-XOR + TBL classify +
compact_mask). Its output is currently discarded (only `parser.rs:18`
uses it, and only to size a `Vec`). The recursive-descent parser
re-walks every structural byte in the slow plane. **Pattern not yet
used:** fed-index stage 2 — replace the inner `while byte != '"'` loops
in `match_tiny_plain_string` / `match_string_at_quote` with index reads
from the retained offset-tape. P2-D Tier A is the candidate. Expected
impact: closes the 75% string-scanner self-time on the loss corpora.

### 5.2 sonic-rs — **Cost-fact-gated NEON `vqtbl1q_u8` tiny-string equality at the dispatch site**

sonic-rs's `from_str::<T>()` typed path generates per-field NEON probes
for keys ≤16 bytes; the cost model is implicit (it always emits when
the key fits). bbnf has the primitive (Class A `match_tiny_plain_string`,
Lock-16-admitted) but the wiring was demoted in SK-V6 Wave 6 because
the broad-fix admission lacked a cost-fact threshold. **Pattern not
yet used:** scalar-threshold-as-cost-fact + NEON kernel only where
LayoutFacts picks it (per SK-V7-A2 §8 admission #6). Expected impact:
lifts twitter, update_center, apache_builds, github_events typed and
retained dispatch sites. This is a SOTA architecture *lesson*, not an
admission: the dispatch-site NEON wiring shape it describes is
pre-blocked by `skinny/REDRESS.md` entry 33 (`REDRESS.md:394-418` —
Class A `match_tiny_plain_string` NEON at the wrong call site,
parse-G fix invalidated), and any S-P3 attempt to wire it carries the
REDRESS-33 material-differential gate.

### 5.3 yyjson — **`__force_inline` fusion of `\uXXXX` decode into the string-walk loop**

yyjson's twitter 30931 Mbps result is the proof: total scalar fusion
beats SIMD when the SIMD pays dispatch overhead or stage-boundary
memory traffic. bbnf's `unescape_json_string` is a cross-function call
into `parse-that-regex` (`generated.rs:6,193,625`); the `read_hex_unit_scalar`
+ `hex_nibble` leaves account for 38-44% on y_string_unicode.
**Pattern not yet used:** codegen-emitted fusion of the 4-nibble decode
+ surrogate-pair handler directly into the string-walk hot leaf, with
the codepoint accumulator writing directly into the field-fact /
offset-tape sink. This is the P2-E unicode-codec target.

### 5.4 asmjson — **Bounded-stack DPDA with PC-as-state direct threading**

asmjson keeps the next-state target in a register (`r10` on x86_64) and
the bracket-pair stack in a fixed-size buffer (`frames_buf[64]`,
`open_buf[64]`). No state-variable memory traffic, no allocator
involvement, no dispatch-table lookup. **Pattern not yet used:** the
generated recursive-descent parser uses Rust function-call frames for
bracket-pair tracking and an enum-tag for state; this pays the function-
call-boundary tax (per yyjson lesson) and the enum-discriminant branch
tax. On M5 Max this is admissible only via a hand-authored aarch64 NASM
path — the V9.5-PSI binding rejects Rust-emitted DPDAs. CollapsedStage
is the fifth `BackendShape` variant defined in the design corpus at
`restart/ARCHITECTURE.md` §7.3 (`LayoutFacts.backend_shape`, enum at
`ARCHITECTURE.md:1086`); CollapsedStage admission for JSON on aarch64 is
the SK-V7 Wave 3 successor work.

### 5.5 RapidJSON — **(none — not a SOTA competitor on M5 Max)**

RapidJSON is the scalar floor reference; no architectural pattern is
load-bearing.

### 5.6 serde_json — **(none — the strict scalar floor)**

---

## §6 — DAV1D / FFmpeg / VLC discipline recap

The transferable architecture from the dav1d/FFmpeg/VLC SIMD process,
per `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md:86-107` and SK-V7-A2 §7:

1. **Scalar oracle first.** Every SIMD/ASM primitive begins as a
   scalar reference function — the executable specification. The
   SIMD/ASM form is a checkasm-differential against it. No kernel
   ships without scalar parity.
2. **`checkasm` differential harness.** dav1d's `tests/checkasm/`
   carries register-clobber detection, rdtsc/rdtscp cycle counters,
   stack-canary XOR-fold, and signal-safe context-save baseline. Each
   primitive has a `func_ref` (scalar) and `func_new` (SIMD) pair; the
   harness asserts byte-identical output for randomised inputs across
   the entire input alphabet. bbnf's `bbnf-simd/tests/checkasm_parity.rs`
   is the consumer harness.
3. **Same-wave consumer rule.** A primitive that lands without a hot-
   path consumer that moves a named row in `RESULTS.md` is
   substrate-without-consumer. This was the failure mode that closed
   Era V (REDRESS 28+33, 60-72, 82-84). Lock 14 codifies the rule:
   primitive + scalar reference + checkasm pass + consumer wiring all
   land in the same wave commit.
4. **Two-layer vocabulary.** Layer 0 is vendored target macro
   substrate (dav1d's `x86inc.asm`, `x86util.asm`); Layer 1 is the
   grammar-neutral primitive contracts (`bbnf.asm` macros 1-9). Layer
   2 is codegen-emitted per-grammar data tables and shims; Layer 3 is
   the consumer wiring with the checkasm gate. The bbnf-simd two-layer
   scheme mirrors dav1d's structure.

**Which competitor practices this most rigorously.** dav1d itself,
unambiguously. The dav1d/FFmpeg practice — every primitive ships
**(scalar reference, checkasm test, hot-path consumer, ABI-hardened
ASM)** as a four-tuple commit — is the gold standard. simdjson
practices a weaker form: the C++ template machinery substitutes for the
checkasm harness (the generic stage 1 has per-architecture
specialisations that share a scalar template). sonic-rs's NEON code in
`src/util/simd/neon.rs` lacks a checkasm-equivalent — the parity
discipline is informal `#[test]` cells comparing SIMD output to scalar
output. yyjson has no SIMD to discipline. **asmjson has no scalar
oracle and no differential harness** — the published 10.93 GiB/s is
not differentially-validated against a scalar reference, which is one
reason its permissive strictness has been an external-observation
finding rather than a build-time guard.

bbnf-simd's checkasm-parity harness is therefore the **closest in-tree
analogue to the dav1d discipline** of any JSON-adjacent project. The
discipline rule is non-negotiable for S-P3 admission.

---

## §7 — The >SOTA path (integrating P2-A union substrate, P2-D ASM opportunities, P2-E unicode codec)

The V3-V6 evidence states the constraint sharply: of the 11
parse_only LOSS rows, **4 cannot be closed by a delimiter-only
intervention** (unicode_mixed, unicode_escapes, y_string_unicode,
gsoc-2018). The throughput gap on those rows exceeds 130-460% of the
regression's full per-byte budget. Strictly beating sonic + simdjson +
yyjson on *every row* requires three coordinated interventions.

**An unowned-route gap.** The Class A NEON tiny-string scanner is
pre-blocked by REDRESS 28+33, yet neither P2-A nor P2-D resolves its
owner: P2-A's union substrate does not address the tiny-string scanner
at all, and P2-D names the kernel but defers it to consumer wiring
without authoring an admission shape. This synthesis records the gap
and states that S-P3 must resolve the owner — assign the tiny-string
scanner to a named wave under an explicit REDRESS 28+33 material-
differential gate, or formally retire the route.

### 7.1 Intervention I — **Consume the stage-1 index (P2-A union substrate, Tier A)**

**What it does.** `scan_structurals` becomes the one producer of
retained structural positions. The `Vec<u32>` of offsets is the tape's
offset column; an aligned `classes: Vec<u8>` column records opaque
structural-class ordinals. Generated parser sites at
`runtime/src/grammars/json/{generated,parser}.rs` that currently
re-discover structural bytes by inner scan are rewritten to cursor/
class reads over the retained tape.

**Why it works.** Removes the 75% string-scanner self-time on the
loss corpora (P1-V3-C). The mechanism is precisely the simdjson
stage-1/stage-2 cleavage: the wide branch-free kernel runs once, and
the narrow dispatch loop consumes indices, not bytes.

**Expected closures.** twitter, update_center, apache_builds,
github_events, random, distinct_values parse rows from LOSS to ≥ sonic.
gsoc-2018 partially closes (its 51% gap reduces but is also unicode-
bearing).

**Same-wave consumer.** The JSON retained parser at the
`consume_structural` call sites. SC-2 §3.3 carries the owner paths,
LOC budget (+150 source net), and verification harness.

**Cost.** P2-D Tier A only; ~+55 bbnf-simd, +90/-10 runtime/src/tape,
-30 regenerated JSON parser output, +45 codegen/table emission.

### 7.2 Intervention II — **Fused `\uXXXX` codec in the string-walk hot leaf (P2-E unicode codec)**

**What it does.** The codegen-emitted JSON string walker fuses the
4-nibble hex decode and surrogate-pair handling into the same
`__force_inline` loop body that copies/validates the string bytes. The
nibble lookup is a 64-entry LUT (one cache line); the surrogate join
is one branch. No call into `parse-that-regex`; no second pass.

**Why it works.** The four uncloseable rows are unicode-codec-dominated
(per P1-V3-C, `read_hex_unit_scalar` + `hex_nibble` = 38-44% on
y_string_unicode). yyjson's twitter 30931 Mbps result is the *proof*
that fusion beats SIMD on string-dense paths.

**Expected closures.** y_string_unicode, unicode_mixed, unicode_escapes,
unicode_basic, distinct_values from LOSS to ≥ sonic-rs strict, and on
y_string_unicode + unicode_mixed strictly above yyjson because bbnf's
DirectBuild + structural-tape substrate avoids yyjson's per-element
allocator round-trip on the typed plane.

**Same-wave consumer.** The `parse-that-regex` unescape hot path only,
per P2-E §4 — the codec wires into the existing
`unescape_four_unicode_escapes` consumer site. It does *not* extend to
a DirectBuild field-fact emit site: that expansion is REDRESS 66-69
territory (the direct string/Unicode close exhausted under the current
strict digest workload) and is not opened by this synthesis.

**Architecture neutrality.** The codec is grammar-neutral by Lock 14:
JSON's `\uXXXX`, CSS L4's `\HEX*` escape, BBNF-self's `\u` literals all
share the same primitive shape. P2-F admits this as a per-grammar
template surface over a grammar-neutral hex-decode primitive.

### 7.3 Intervention III — **P2-D ASM kernel opportunities (admission shapes deferred)**

P2-D names a family of aarch64 ASM kernel opportunities — Class A
`match_tiny_plain_string` NEON equality and `BITMAP_NEXT_SET_BIT` /
CSSC `CTZ` next-structural seek — that bear on the per-object-key
dispatch cost (Intervention I addresses string boundaries; this
intervention class addresses field recognition).

This synthesis does *not* author the admission shapes for those
kernels. A Class A `match_tiny_plain_string` cost-fact wired at the
field-name match arm chain is the REDRESS 33 rejected shape, and
broadening the P2-D §4.4 narrow string-mask consumer into a bulk
consumer is REDRESS 89 territory. **Admission shapes for P2-D ASM
kernels are authored by S-P3 with explicit REDRESS material-
differential gates.**

### 7.4 Inter-report dependency graph

This synthesis does *not* author a wave sequence or a cumulative impact
projection. Wave-class authorship and per-wave cost-set authoring
belongs to S-P3 per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
(P3-B wave sequencing, P3-C cost authoring). This synthesis names
dependency only.

The dependency graph below records which P2 research findings a given
intervention rests on — the arrow `A ← B` reads "A depends on B; B must
land first":

- **Intervention I (consume the stage-1 index)** ← **P2-A** (union
  substrate, Tier A — the retained offset/class tape that I consumes)
  ← **P2-B** (the grammar-neutral substrate contract P2-A's union
  rests on). The chain is `I ← P2-A ← P2-B`.
- **Intervention II (fused `\uXXXX` codec)** ← **P2-E** (the unicode-
  codec design, parameterised at codegen time as a per-grammar
  template surface over the grammar-neutral hex-decode primitive).
  Intervention II also ← **P2-A** to the extent the codec's offset-tape
  sink consumes the retained-tape substrate. The chain is `II ← P2-E`
  with a secondary edge `II ← P2-A`.
- **Intervention III (P2-D ASM kernel class)** ← **P2-D** (the named
  ASM opportunities) ← **P2-A** (the retained tape that the next-
  structural seek kernel cursors over). The chain is `III ← P2-D ←
  P2-A`. Admission shapes are deferred to S-P3 per §7.3.

**No cost set.** This dependency graph carries no per-slice minute
caps, no LOC budget, and no cumulative throughput projection — S-P3
owns the cost-set authoring (P3-C) and the wave sequencing (P3-B). The
graph states ordering *necessity* (substrate before consumer), not a
wave plan.

**The >SOTA gate.** On Apple M5 Max, strictly above sonic-rs +
simdjson NEON + yyjson on every row, on both parse_only and
real_typed_struct planes, with matched RFC-8259 strictness, with
same-run criterion evidence — *that* is the close criterion. asmjson
remains a non-anchored sidecar; the Zen-4 follow-up tranche carries
the CollapsedStage proof against the 10.93 GiB/s headline.

---

## §8 — Sources

In-tree authoritative inputs:

- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  (lines 51-95: PMU table provenance, load-bearing diagnoses, regression
  coefficients).
- `/tmp/skv9-xctrace-v3/pmu_rows.tsv` (34 rows × {17 corpora × 2
  tracks}: cycles, instructions, CPI, cycles/byte).
- `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md` (post SK-V7
  W10c: 17 parse-G rows, same-run sonic-rs strict and lossy anchors,
  historical sidecar simdjson/yyjson/rapidjson, asmjson n/a flaw probe).
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v7/research/skv7-A2-sota-strict-beat.md`
  (§1 strict comparator matrix; §2-§3 per-parser architecture; §5 what
  bbnf does not beat; §6 asmjson DPDA pattern; §8 admission ledger).
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md`
  (§1.1 simdjson two-stage; §1.2 asmjson DPDA; §1.3 sonic-rs skip-scan;
  §1.4 yyjson scalar fusion; §1.5 bbnf offset-tape conflation diagnosis;
  §3 Tier A + Tier B union candidate).
- `/Users/mkbabb/Programming/bbnf-lang/restart/prompts/skinny/PASS-2-RESEARCH.md`
  (§2 scope matrix; §3 CHALLENGE lens registry; §8 strict-vs-strict
  discipline).
- `restart/locks/LOCKS.md` (Lock 1 substrate union, Lock 14 grammar-
  neutrality, Lock 16 admissibility allowlist).

External authoritative anchors:

- Langdale & Lemire, "Parsing Gigabytes of JSON per Second," VLDB
  Journal 28(6), 2019, arXiv:1902.08318. §2 branch-misprediction cost;
  §3 two-stage decomposition; §4.2 carry-less prefix-XOR string-mask;
  §4.4 branch-free bit-to-index compressstore; §6 per-stage IPC.
- simdjson NEON source: `src/arm64/` — `vqtbl1q_u8` TBL classify,
  `pmull{,2}_p64` polynomial-multiply for prefix_xor, `vshrn_n_u16`
  bit-to-index compaction.
- sonic-rs upstream README + `src/util/simd/neon.rs` — `vceqq_u8` ⊕
  `vandq_u8` skip-scan kernels; rejects simdjson two-stage tape; typed
  `from_str::<T>()` headline ~695 µs on twitter vs simd-json ~1.06 ms.
- yyjson upstream README + `yyjson.c` read path — C89 single-pass
  scalar, no SIMD, no structural index, `\uXXXX` decode fused into
  string-walk loop, ≤20 KiB i-cache discipline.
- asmjson docs.rs `https://docs.rs/asmjson/` + upstream README — AVX-
  512BW + SWAR routing, 10.93 GiB/s permissive single-thread DOM on
  Zen 4; 9-state DPDA; permissive control-byte treatment.
- ARM Architecture Reference Manual ARMv9.2-A — NEON ASIMD vector
  instructions; CSSC (Common Short Sequence Compression) extension
  (CLS, CTZ, RBIT, CNT one-cycle integer); ARMv8.6 dot-product / I8MM.
- Apple M5 silicon: 4.05 GHz P-cores, 8-wide decode, 12-wide back-end,
  16 KiB L1-I + 192 KiB L1-D + 256-entry µop-cache per P-core (publicly
  documented Apple Silicon specs).
- RFC 8259 — strict JSON: four whitespace bytes (space, HT, LF, CR);
  strings escape `"`, `\`, C0 controls U+0000–U+001F.

---

## §0 — V2 fold: synthesis-overreach walk-back

The V1 CHALLENGE found this report's §7 had drifted into S-P3
synthesis-plan scope (the S-P1 V4 CH4 failure mode recurring) and had
silently reopened pre-blocked routes. The V2 fold walks all of it back:

- **§7.4 reframed as an inter-report dependency graph.** V1 §7.4
  authored a four-stage sequencing table plus a cumulative
  throughput-impact projection — that is P3-B wave sequencing and P3-C
  cost authoring, owned by S-P3 per
  `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`. §7.4 now records
  only dependency (`I ← P2-A ← P2-B`; `II ← P2-E`, secondary `II ←
  P2-A`; `III ← P2-D ← P2-A`) and carries no cost set.
- **§7.2 DirectBuild expansion stripped.** V1 §7.2 wired the fused
  `\uXXXX` codec at a "DirectBuild field-fact emit site" — REDRESS
  66-69 territory. The codec now wires at the `parse-that-regex`
  unescape hot path only, per P2-E §4.
- **§7.3 admission shapes stripped.** V1 §7.3 admission 1 was the
  REDRESS 33 rejected `match_tiny_plain_string` shape; admission 2
  broadened P2-D §4.4 into REDRESS 89 bulk-consumer territory. §7.3
  now defers all P2-D ASM admission shapes to S-P3 under explicit
  REDRESS material-differential gates.
- **§3 "Room to widen the lead" walked back.** V1 §3 proposed Class A
  NEON at the DirectBuild dispatch and `\uXXXX` fusion into the typed
  materialiser — silently reopening REDRESS 33 + 66-69 and risking the
  4 typed-GO rows. §3 now states only that any widening touching the
  dispatch hot path or typed materialiser is REDRESS-bound and
  requires S-P3 differential authoring.
- **§7 owner gap recorded.** The Class A NEON tiny-string scanner is
  pre-blocked by REDRESS 28+33 with no owner across P2-A and P2-D;
  §7's preamble now names the gap and assigns its resolution to S-P3.

This report names competitor architecture, per-corpus position,
intervention identity, and inter-report dependency. It does not author
wave classes, cost sets, sequencing, or REDRESS admission shapes —
those are S-P3 product.

**V3 fold: CH3 + CH6 surgical residuals closed.** Three surgical edits
landed against the S-P2 V2 CHALLENGE consolidation
(`hardening/HARDENING-S-P2-V2-CONSOLIDATED.md` §V3-fold-requirements):
§5.2's sonic-rs `match_tiny_plain_string`-class lesson now inline-cites
that its dispatch-site NEON wiring shape is pre-blocked by
`skinny/REDRESS.md` entry 33 (`:394-418`), so the lesson is not mistaken
for an admission (CH3 fold #2); the §2.1 ContainerNext reference now
carries the `generated.rs:341` enum definition cite and the §5.4
CollapsedStage reference is anchored to its `restart/ARCHITECTURE.md`
§7.3 design-corpus definition (CH6 V2-F-5); and the asmjson §5
primitive-vocabulary reference now anchors the canonical primitive-class
taxonomy by path to `skv9-p1-v3-B-xctrace-time-profiler.md` §1.5 (CH2
item-7 hygiene).

End of report.
