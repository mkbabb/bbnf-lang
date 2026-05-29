# SK-V17 P2-A: SOTA Comparator Teardown

Pass: S-P2 Research. Cycle: V2.
Date: 2026-05-29.
Scope: Architecture teardown of the SOTA parsers (asmjson / sonic-rs / simdjson / yyjson) on the JSON plane and the CSS comparators (lightningcss / cssparser) on the benched plane. For each: structural-classification strategy, number/string fast paths, tape/DOM/on-demand output plane, and the strict-vs-strict comparator discipline (§8.1). Name precisely what each does that bbnf does not — keyed to the S-P1 hot leaves. SELECTS nothing; grounds the candidate pool.
Output: this file.
P1 hot-leaf antecedents: `CssFullParser::find_component_delim` (`css_l4_declaration_values/generated.rs:288`, 56.52–59.24% self, scan); `CssFullParser::consume_balanced_at` (`generated.rs:320`, 10.31–11.05%, structural-over-scan, folds into the SAME byte-class scan target); `generated::emit_fact_stream` (`generated.rs:5`, 24.59–25.01% self + 91% of the ~58–64% syscall/malloc floor it reaches, string/alloc); `generated::push_ascii_lower_hex` (`generated.rs:628`, 8.98–9.11%, FNV/hex diagnostic — NO primitive); the syscall+heap allocator floor (libsystem_kernel/malloc, ~58–64% on the fact_stream plane, tape/alloc). JSON antecedent for the lazy-view route: `json/scan.rs` (NEON structural scan; fn opens `:207`, `classify_structural_terminator_block_from_table` call `:217-218`, `prefix_xor`/`escape_mask` `:237-239`) + `json/value.rs:143` (`value_from_ref` lazy `ValueRef` projection).
Lock surface: both (Lock 1 substrate-union for the output-plane teardown; Lock 14 grammar-neutrality for every primitive's CSS/JSON-shared verdict).

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

### 1.0 — The benched ground truth this teardown grounds against (P1-E / P1-F locked)

The S-P1 profile (LOCKED, `HARDENING-S-P1-V4-CONSOLIDATED.md §3.1`, commit `0ae1caa52`)
inverts the SK-V16 "~14× slower" narrative. At N=200 cold per-corpus median on the
canonical harness (`css_canon_bench.rs`, `assert!(n >= 50)` :250), the **LOCKED V4 band**
(load-bearing; the figures every downstream selection answers to):

- The **recognition** plane (`track1_full_parse`) already BEATS lightningcss full-CSSOM
  by **2.01–3.09×** every corpus (bootstrap **2.05×**, tailwind **3.09×**, material
  **2.05×**, animate **2.01×** — `§3.1` within-harness ratio table). But it is NOT
  preserve-rich-ast — it counts rules/at_rules/qualified/decls (`generated.rs:91-99`) and
  materializes nothing. It is a *masking probe*: the scanner has ~2–3× headroom over
  lightningcss. (RUN-SPREAD, explicitly NON-load-bearing per `§3.1`: the 3-run full÷lcss
  spread is bootstrap 2.12/2.25/2.05, tailwind 3.50/3.00/3.09, material 2.37/2.11/2.05,
  animate 2.06/1.97/2.01 — every corpus stays decisively >1.0× every run; the LOCKED V4
  median band above is the load-bearing figure, the spread is disclosure only.)
- The **typed fact-stream** plane (`track1_fact_stream`, the live benched Track 1) lands
  at **0.60–0.77× lightningcss** (bootstrap 0.77×, tailwind 0.67×, material 0.69×, animate
  0.60× — LOCKED V4 `§3.1`) — a sub-2× materialization-plane gap, not an order of
  magnitude. The gap is **String building**: ~58–64% syscall+malloc floor (91% reached
  FROM `emit_fact_stream` String growth, P1-E caller-walk) + ~25% in the `push_str`
  accumulator + 8.98% in FNV hex.
- The cost-density quantum: fact_stream is **214.56–364.51 instr/byte** vs full_parse's
  **46.46–57.72 instr/byte** (LOCKED V4 PMU `§3.1`) — a **~4.4× instr/byte tax** that is
  entirely String+alloc, not scan.
- Comparator absolute medians (LOCKED V4 `§3.1`): lightningcss full-CSSOM **833–1261 Mbps**
  (bootstrap 1110.169, tailwind 833.786, material 1261.148 with a 160.300 cold outlier,
  animate 1237.346); cssparser token-scan 1731–3248 Mbps.

This is the load-bearing frame for the teardown: SK-V17's task is to keep the scanner's
recognition headroom while replacing the String materialization floor with a tape-append
+ lazy-`ValueRef` plane that costs less than `String`. The SOTA parsers below are torn
down for exactly the two levers the profile names: (a) **how each materializes without a
String accumulator** (the output-plane teardown → the tape lever), and (b) **how each
classifies structurals without a byte-at-a-time `delimiters.contains(&byte)` membership
scan** (the scan teardown → the NEON byte-class lever, gated behind the tape).

### 1.1 — simdjson (the two-stage DOM/tape + On-Demand lazy reference)

**Structural classification.** simdjson's Stage 1 is a SIMD structural+UTF-8 discovery
pass that produces a contiguous index of every structural byte, decoupled from Stage 2
tape construction (Langdale & Lemire, VLDB 2019 arXiv:1902.08318; simdjson
`doc/parse_many.md:54-57` @ `79bbba3e`). Stage 1 emits a transient structural index;
Stage 2 reads that index to build the tape. **What bbnf does not do:** the benched CSS
recognition path does byte-at-a-time membership (`find_component_delim` :295
`delimiters.contains(&byte)` over a 3-byte slice + per-byte `match` :298, `generated.rs:288`)
— there is NO block-wide SIMD structural classification on the CSS path (P1-E §2.5: "the
`dispatch` vehicle … appears nowhere — zero SIMD on the CSS path"). simdjson's Stage 1 is
exactly the block-wide byte-class primitive the JSON path ALREADY runs (`json/scan.rs`,
fn opens `:207`, the `classify_structural_terminator_block_from_table` call at `:217-218`,
the `prefix_xor_64`/`escape_mask_64` body at `:237-239`). The CSS scan is the unported
analogue. (Per the totality 2A teardown `2A-sota-landscape.md:51`, T2A-V1-SOTA-JSON-001:
Stage 1 is a *transient structural projection* consumed by ONE DOM/tape builder —
admissible only as a same-loop mask, NOT a retained class column; Lock 1 / CH5.)

**Output plane.** simdjson has TWO planes. (a) **DOM/tape** — Stage 2 builds a flat tape
of (type, offset/value) records; the tape IS the materialized structure (no per-node
heap box). (b) **On-Demand** — a lazy forward iterator over source text that parses values
*as used* and skips unused values (`doc/basics.md:344-350` @ `79bbba3e`,
T2A-V1-SOTA-JSON-002). **What bbnf does not do on CSS:** the benched CSS Track 1 emits a
fact-stream `String` (`emit_fact_stream`, `generated.rs:5`), the diametric opposite of a
tape — it serializes to a growing heap String (24.59% self + the alloc floor it reaches),
materializing NOTHING queryable, paying String-grow cost no SOTA parser pays. simdjson's
On-Demand plane is the architectural template for SK-V17's lazy-`ValueRef` route: bbnf's
JSON already does this (`json/value.rs:143` `value_from_ref` reconstructs `JsonValue` from
a `(Tape, cursor)` on demand) — the CSS rider does NOT yet exist (the gating artefact,
SYNTHESIS §0.3).

**Number/string fast paths.** simdjson Stage 2 only touches source bytes inside the
primitives: `stringparsing::parse_string`, `parse_digit`,
`parse_decimal_after_separator` (SOTA-BEAT-DESIGN.md:61). Strings carry a scan-time
`HasEsc` flag so the non-escaped path borrows bytes directly without an escape-decode
pass (SOTA-BEAT-DESIGN.md:34,59). **What bbnf does not do on CSS:** the CSS recognition
path does NO number decode (it counts, P1-E §2.5: "no float parse") and NO unicode decode
(treats `>=0x80` as a name byte, `generated.rs:404`); the typed fact-stream plane decodes
into a String, not into a borrowed-span value. The lazy-`ValueRef` CSS rider must decode
dimensions/colors on demand from `(Tape, cursor)`, mirroring simdjson's
touch-source-only-inside-primitives discipline.

### 1.2 — sonic-rs (targeted-SIMD-leaves + direct typed deserialization)

**Structural classification.** sonic-rs explicitly REJECTS the simdjson two-stage SIMD
architecture and instead applies targeted SIMD at four leaves: long-string scan, float
fraction parsing, field lookup, and whitespace skip (`README.md:60-66` @ `03545a95`,
T2A-V1-SOTA-JSON-003). It achieves the structural-index-consumption shape via LTO fusion:
all SIMD primitives inline into the one `parse_object`/`parse_array` driver
(SOTA-BEAT-DESIGN.md:61). **What bbnf does not do:** the CSS scan is a non-fused
recursive-descent (`find_component_delim` → `consume_balanced_at` recursion, each walking
the same byte-membership inner loop 2–3× per declaration body, P1-D §2.5 /
`HARDENING-S-P1-V4 §3.3`). sonic-rs proves the alternative-to-two-stage shape: targeted
SIMD leaves + LTO fusion, which is the SK-V17 Lock 15 build-profile route
(SOTA-BEAT-DESIGN.md:61). This grounds the single byte-class scan leaf (not a whole-parser
re-architecture) as the correct primitive grain.

**Output plane.** sonic-rs direct struct deserialization (`README.md:78-90`,
T2A-V1-SOTA-JSON-004) is a TYPED-DIRECT plane: parse straight into a `#[derive(Deserialize)]`
struct, distinct from untyped `sonic_rs::Value`/DOM. **What bbnf does not do on CSS:** the
benched Track 1 typed plane builds a String, not a typed value — sonic-rs's typed-direct
plane is the materialization-without-String discipline. (Caveat per §8.1: the SK-V6
finding is sonic-rs's `utf8_lossy` is a permissive-plane flaw probe; only its strict typed
path is a fair bar. Carried below.)

**Number/string fast paths.** sonic-rs's four SIMD leaves are the grammar-neutral leaf
*menu* the totality 2A defends (`2A-sota-landscape.md:71` assertion 3: byte-set classify,
long-string scan, numeric fragment scan, field/property lookup, whitespace/trivia skip).
Of these, ONLY the byte-set classify has a benched CSS antecedent (the
`find_component_delim` scan, ~69%); the others have NO CSS self-time on the current planes
(P1-E §2.5, anomaly 4: no number/unicode/dispatch hot leaf). This is the CH1/CH6 discipline:
sonic-rs offers five candidate leaves, but only one grounds against an SK-V17 hot leaf —
the rest are JSON-context candidates without a CSS P1 antecedent and are NOT P2-A
candidates (they may resurface once the typed lazy-`ValueRef` path decodes dimensions, a
plane P1 could not measure; P1-E anomaly 4(a) / SYNTHESIS C4b gate).

### 1.3 — yyjson (the no-SIMD scalar DOM/value baseline — the refutation anchor)

**Structural classification.** yyjson is ANSI C, NO explicit SIMD, with an ILP /
branch-predictor performance posture (`README.md:10-18` @ `d6085270`,
T2A-V1-SOTA-JSON-005). It is SOTA on selected corpora WITHOUT any SIMD. **What this
refutes:** the claim that beating lightningcss REQUIRES NEON. yyjson proves scalar ILP can
be SOTA-competitive — which is consonant with the P1 ordering (SYNTHESIS §0.4 NEON gate /
P1-E anomaly 3): the SK-V17 lever order is **tape FIRST (the String floor is ~58–64%,
grammar-neutral, no SIMD), THEN NEON on the surviving scan**. yyjson is the empirical
proof that the tape lever alone (no SIMD) can move the bench materially, and that the NEON
scan lever is the second-order win, not the prerequisite.

**Output plane.** yyjson builds a DOM/value tree — a contiguous value-array DOM, not
per-node heap boxes. **What bbnf does not do on CSS:** the eager-typed CSS plane that WOULD
build a value tree is AZ-IV pre-blocked (the 118× regression, SYNTHESIS §0.4; P1-F outcome
**K**). yyjson shows the DOM can be cheap if it is a contiguous array (the tape shape), not
an eager `Box`-per-node tree — which is precisely the lazy-`ValueRef`-over-`Tape` route
the contract mandates over the AZ-IV eager tree.

**Number/string fast paths.** yyjson is the **scalar reference shape** the dav1d/Lock 16
process (P2-B) requires for every SIMD primitive: a yyjson-shape scalar baseline is the
oracle a NEON kernel is a checkasm-differential against (`2A-sota-landscape.md:55`,
T2A-V1-SOTA-JSON-005: "scalar-delegated baseline, no new bbnf LOC"; LAC-05). It is NOT a
primitive itself — it is the admission anchor.

### 1.4 — asmjson (x86 AVX-512 collapsed-stage FSM — REDRESS/host-blocked)

**Structural classification.** asmjson is an AVX-512 collapsed-stage 9-state DPDA with
direct-threaded dispatch via `jmp [r10 + state*8]` and a hardware-bounded explicit stack
(`ARCHITECTURE.md:1276,1284`; SOTA-BEAT-DESIGN.md §5.1). Its instruction footprint is
minimal: only `vpcmpeqb, kmovq, vpcmpub, korq, vmovdqu8, tzcnt` — ZERO of the AVX-512
esoterica (`MASTER-PLAN.md:702`). **What this is for SK-V17:** a **host-blocked
non-candidate**. asmjson is AVX-512-only; the SK-V17 host is Apple M5 Max aarch64 (NO x86,
NO AVX-512, NO SVE — SYNTHESIS §0.4, dispatch pre-block). The `CollapsedStage` lowerer is
`NOT-ADMITTED: x86-only; aarch64 mechanically refused` (`ARCHITECTURE.md:1206`). asmjson is
torn down here ONLY to document the strict-comparator boundary (§8.1) and to record that
its PC-as-state pattern is unreachable in LLVM-emitted aarch64 Rust (`ARCHITECTURE.md:1284`:
"LLVM cannot fold an indirect-dispatch state walk back into PC-as-state form"). **No P2-A
candidate derives from asmjson** — every asmjson route is CH3-REJECT on host grounds. The
only transferable lesson is the *two-layer vocabulary* factoring (Layer-0 vendored /
Layer-1 grammar-neutral macros, SOTA-BEAT-DESIGN.md §5.2), which is P2-E's scope, not a
primitive here.

### 1.5 — lightningcss (THE fair CSS >SOTA bar — full L2 CSSOM)

**Output plane.** lightningcss parses a stylesheet into a rule list + typed
property/value structures, then serializes/minifies from that model (`README.md:10-12`,
`src/stylesheet.rs:74-91,122-207`, `src/properties/mod.rs:1-18,81-89` @ `ec165294`,
T2A-V1-SOTA-CSS-003). It MATERIALIZES a full owned CSSOM — proven by P1's profile of the
comparator's own flame: ~30% typed-node build+drop (`parcel_selectors::parser::parse_selector`
5.04%, `lightningcss::declaration::parse_declaration` 4.16%,
`drop_in_place::<cssparser::Token>` 3.95%, `PropertyId::from_name_and_prefix` 2.39%;
`HARDENING-S-P1-V4 §3.3` comparator flame). This is the SAME plane SK-V17 Track 1 must
reach (typed CSSOM via lazy `ValueRef`), so it is the fair bar (alphaB §0; SYNTHESIS §0.6).
LOCKED V4 per-corpus median lightningcss: **833–1261 Mbps** (`§3.1`: bootstrap 1110.169,
tailwind 833.786, material 1261.148 [160.300 cold outlier], animate 1237.346). **What bbnf
does:** the benched Track 1 emits a fact-stream String at **0.60–0.77×** this bar (LOCKED
V4 ratio band) — the materialization-plane gap the tape lever closes.

**Structural strategy.** lightningcss is built ON cssparser (it consumes cssparser's
token stream then builds typed values) — ~38% of its self-time is the cssparser tokenizer
(`consume_name` 8.92%, `skip_whitespace` 5.88%, `next_token` 5.36%;
`HARDENING-S-P1-V4 §3.3`). lightningcss sits at ~38% of cssparser throughput (~2.6–3×
slower), the legitimate materialization tax a SOTA full-CSSOM parser pays (alphaB §4). This
is the headroom budget: bbnf's recognition scanner is ~2–3× FASTER than lightningcss's
full pipeline (LOCKED V4 2.01–3.09×), so the entire deficit is materialization the tape
must do cheaper than lightningcss's cssparser-tokenize + typed-build.

### 1.6 — cssparser (token-scan flaw probe — NOT a >SOTA bar)

**Output plane.** cssparser is a CSS Syntax tokenizer over borrowed `&str`; it emits a
flat token stream with NO rule/selector/declaration tree and NO typed values
(`src/lib.rs:12-28`, `src/parser.rs:256-264` @ `4c494864`, T2A-V1-SOTA-CSS-001/002;
benched probe `css_canon_bench.rs:282-403` `CssparserFullParseProbe`, which iterates
`StyleSheetParser` + `RuleBodyParser` + `consume_component_values` and RETURNS `()` —
materializes nothing). It is 1731–3248 Mbps (LOCKED V4 `§3.1`), ~2–6× the fact-stream
Track 1. **Discipline (§8.1):** cssparser is the CSS analogue of the SK-V6 sonic-rs
`utf8_lossy` finding — a faster comparator that retains less is NOT a fair >SOTA bar
(alphaB §0, SYNTHESIS §0.6). It is the **structural-equality parity ORACLE** (8-field:
rules=10136 / style=9561 / sel=9561 / decls=20043, EXACT match, `1c5bd7a25`) and a
plane-disclosed reference. Beating cssparser is NOT a SOTA claim; beating lightningcss is.
**What this fixes:** the SK-V16 SPEC named cssparser as the Track 1 admit comparator — a
*correctness/parity* gate, NOT a speed >SOTA gate (alphaB §0). P2-A keeps the split:
cssparser = parity oracle; lightningcss = the speed bar.

### 1.7 — The synthesis: precisely what bbnf does not do, keyed to hot leaves

| SOTA parser | The thing it does | The bbnf hot leaf that proves bbnf does NOT | bbnf-CSS gap |
|---|---|---|---|
| simdjson Stage 1 | block-wide SIMD structural index | `find_component_delim` :288 byte-at-a-time `delimiters.contains(&byte)` :295 (56.52–59.24%) | no SIMD structural classify on CSS (P1-E §2.5) |
| simdjson On-Demand / sonic-rs typed-direct / yyjson DOM | materialize WITHOUT a String accumulator (tape / lazy iterator / contiguous DOM) | `emit_fact_stream` :5 (24.59–25.01% self + 91% of the ~58–64% alloc floor) | benched Track 1 IS a fact-stream String (the diametric opposite) |
| simdjson `HasEsc` flag + touch-source-only-in-primitives | lazy decode hints; borrow non-escaped spans | `value_from_ref` (`json/value.rs:143`) exists for JSON; NO CSS lazy-`ValueRef` rider exists | the gating artefact (SYNTHESIS §0.3) |
| sonic-rs LTO fusion | one fused `parse_object` driver | `find_component_delim`+`consume_balanced_at` non-fused recursion walking the body 2–3× | tokenize-once opportunity, REDRESS-53-bounded |
| yyjson no-SIMD ILP SOTA | scalar can be SOTA without SIMD | the ~58–64% String/alloc floor is grammar-neutral, no-SIMD-removable | proves tape-lever-first is correct (no SIMD prereq) |

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

Every candidate below traces to a NAMED S-P1 hot leaf (CH1). Each carries the canonical
five-field shape: SHAPE, SCALAR-REF STATUS (SR), CHECKASM-ANALOGUE (CK), SAME-WAVE-CONSUMER
(SWC), ARCH, P1 ANTECEDENT — plus a GRAMMAR-NEUTRAL VERDICT (per the dispatch §2
requirement + CH4 cost discipline). P2-A SELECTS none — this is the SOTA-grounded candidate
pool from which S-P3 draws. The §3 grammar-neutrality verdicts are load-bearing per CH2.

### CP-A1 — Block-wide byte-class structural classifier (eq-set fan for CSS; lo6 table for JSON)

- **Shape.** A 64-byte-block NEON classifier that replaces the byte-at-a-time
  `delimiters.contains(&byte)` membership loop with a single block emit of a `u64` movemask
  of structural-byte positions, fed to a movemask-cascade (`vshrn`/`vsri`/`zip1`, Validark
  interleaved) + `bulk_emit_positions_64` writing the structural offsets into a `Vec<u32>`
  index. **The CSS route is the eq-set fan** `byte_class_from_eq_set_64_neon`
  (`bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33`): for each member of the
  structural set (`set: &[u8]`, len ≤ 8 — e.g. CSS `b";{}():"`), broadcast `vdupq_n_u8`,
  `vceqq_u8` against the four 16-lane stripes, OR-reduce, then `movemask_u8x16`-pack to a
  `u64`. This is a per-member equality fan — it admits ANY alphabet with no slot-collision
  constraint, which is why it is the CSS-correct route.
- **Why NOT the lo6 table route on CSS.** The lo6 `vqtbl4q_u8` 4-table route
  (`SelectedBackend::NeonTbl4`, gated by `lo6_table_admissible` `dispatch.rs:101`) is
  **INADMISSIBLE for CSS**: that gate computes `(byte & 0x3f)` — a **low-6-bit MASK** — and
  rejects any alphabet whose distinct bytes collide in their low 6 bits. CSS's `;` (`0x3b`)
  and `{` (`0x7b`) collide: `0x3b & 0x3f = 0x3b = slot 59` and `0x7b & 0x3f = 0x3b = slot 59`
  (verified against `dispatch.rs:101-115` `seen[slot]` reject). They would NOT coincide
  under true modulo (`0x7b % 0x3f = 0x3c`), but the guard is a mask, not a modulo. So
  `lo6_table_admissible` returns `false` for every CSS structural alphabet and the lo6 route
  falls to `SelectedBackend::Scalar`. Compounding: the wired NEON table impl
  `byte_class_from_table_64_neon` is itself a **scalar passthrough today** (it tail-calls
  `byte_class_from_table_64_scalar`, `aarch64/byte_class_from_table_64.rs:1-4`). Routing CSS
  through the lo6/table path would therefore claim a SIMD win it silently runs scalar. CSS
  routes through the eq-set fan; the lo6 table route is **JSON-only** (JSON's structural
  alphabet `{}[],:"` has no low-6-bit collision and is the live `json/scan.rs:217` path).
- **Scalar-ref status (SR).** **EXISTS.** The eq-set scalar reference is the obvious
  per-byte set-membership scalar (the existing `delimiters.contains(&byte)` IS the scalar
  oracle the eq-set NEON form is a differential against; the bbnf-simd scalar twin lives
  alongside the kernel module). Already a real NEON impl (not a passthrough): the
  `vceqq_u8`/`vorrq_u8` fan is present at `byte_class_from_eq_set_64.rs:33-70`.
- **Checkasm-analogue (CK).** REQUIRED-NEW `checkasm_byte_class_from_eq_set_64`:
  exhaustive-corpus differential of `byte_class_from_eq_set_64_neon` vs the scalar
  set-membership oracle over the 256-byte space × every benched CSS structural alphabet
  (`b";{}"`, `b"{};"`, `b":{};"`, `b"()"`), asserting identical `u64` movemask per block —
  the `classifier_parity.rs` exhaustive-256 vehicle (SOTA-BEAT-DESIGN.md:98). The JSON lo6
  table route is already parity-gated by the live `json/scan.rs` checkasm. The eq-set
  kernel's non-JSON exercise is checkasm corpus-parity ONLY (it is not live in JSON prod —
  JSON uses the lo6 table path), so the eq-set parity gate IS its sole exercise until CSS
  wires it.
- **Same-wave-consumer (SWC).** **PRESENT-AND-NAMED = CP-A4 tokenize-once.** The `Vec<u32>`
  structural index this classifier emits is consumed in-wave by CP-A4 (the
  declaration/selector parse reads the index instead of re-scanning source); there is no
  orphan kernel — the classifier exists only to feed CP-A4's consumption, and CP-A4 exists
  only over CP-A1's index. The pair lands together or neither lands.
- **Arch.** aarch64 NEON. CSS = `byte_class_from_eq_set_64_neon` (`vceqq_u8`/`vorrq_u8`
  per-member fan; ARM NEON intrinsics reference, compare/OR family). JSON = `vqtbl4q_u8`
  4-table classify (Lemire 2019 "Arbitrary byte-to-byte maps using ARM NEON",
  `MASTER-PLAN.md:693`), gated by `lo6_table_admissible` (`dispatch.rs:101`). NO x86.
- **P1 antecedent.** `CssFullParser::find_component_delim` (`generated.rs:288`, 56.52–59.24%
  self, hot at :295 membership + :298 dispatch) + `consume_balanced_at` (`generated.rs:320`,
  10.31–11.05%) — the two fold to ONE byte-class scan target (~69% of recognition self-time;
  `HARDENING-S-P1-V4 §3.3`).
- **SOTA antecedent.** simdjson Stage 1 structural discovery (`parse_many.md:54-57`); the
  same primitive JSON already runs (`json/scan.rs:217`, lo6 table route) — CSS rides the
  eq-set fan analogue. Cross-artefact alignment: P2-C C2 / P2-F §1.2 / P2-D §1.4 all route
  CSS through the eq-set fan, not lo6.

### CP-A2 — Tape-append materialization sink (`push_plain_offset`)

- **Shape.** Replace the fact-stream `String` accumulator with a `TapeBuilder` that appends
  a single branchless `u32` offset per structural event into `self.offsets`
  (`assembler.rs:71` `push_plain_offset`: bounds-check + `self.offsets.push(offset as u32)`,
  one write; cold-path `reserve_offsets_cold` only on capacity exhaustion). Lazy flags
  (`HasEsc`-analogue) patched only when set (`push_offset` :63 → `patch_flags`). O(1)
  checkpoint = `offsets.len()` marker; rollback = truncate (no `split_off`, no `Vec<Vec>`).
- **Scalar-ref status (SR).** **N/A — not a SIMD primitive.** It is a scalar data-structure
  operation; the "reference" is the existing JSON tape usage (`json/scan.rs` emits into the
  same `Tape`/`ValueRef`). No vector kernel ⇒ no checkasm-vector parity needed.
- **Checkasm-analogue (CK).** Not a vector checkasm; the correctness analogue is the
  **cssparser 8-field structural-equality re-proof** (rules=10136 / style=9561 / sel=9561 /
  decls=20043, EXACT, `1c5bd7a25`) — the same differential discipline as a checkasm but on
  the structural-count plane (a tape that appends the right offsets reproduces the 8-field
  oracle byte-for-byte). This is the Checkasm-analogue the dav1d process (P2-B) maps onto a
  non-vector substrate change.
- **Same-wave-consumer (SWC).** **PRESENT-AND-NAMED = CP-A3 lazy `ValueRef` rider.** The
  offsets CP-A2 appends are consumed in-wave by CP-A3 (`value_from_ref`-isomorphic accessor
  walks `(Tape, cursor)` over exactly these offsets). The tape sink is not a write-only
  store — CP-A3 is its reader; the two are the producer/consumer pair of the SAME substrate
  (Lock 1). Neither ships without the other.
- **Arch.** Host-neutral scalar (the `u32` push compiles identically aarch64/x86).
- **P1 antecedent.** `emit_fact_stream` (`generated.rs:5`, 24.59–25.01% self + 91% of the
  ~58–64% syscall/malloc floor it reaches; P1-E §2.4 caller-walk). This is the dominant
  fact-stream cost; the tape append retires it.
- **SOTA antecedent.** simdjson Stage 2 tape (flat (type,offset) records, no per-node box);
  yyjson contiguous value-array DOM. The contrast is direct: both materialize without a
  growing String; `emit_fact_stream` does the opposite.

### CP-A3 — Lazy `ValueRef` typed-value reconstruction (the On-Demand CSS rider)

- **Shape.** A generated accessor that reconstructs each typed CSS node
  (`CssColor`/`CssDimension`/`CssLength`/`CssFunction`/`Selector`/`CssRule`/`CssTypedValue`)
  from a `(Tape, ValueRef cursor)` ON DEMAND — child-position → `ValueRef` child, branch
  tag → meta dispatch, typed leaf → decode-by-type, rule reference → child + recurse. No
  eager per-leaf `Box::new`, no eager value tree. Isomorphic to JSON's `value_from_ref`
  (`json/value.rs:143`, which dispatches `JsonNodeKind::at_cursor(tape, cursor)` into a
  borrowing `JsonValue`).
- **Scalar-ref status (SR).** **N/A — not a SIMD primitive** (it is a codegen-emitted access
  pattern). The reference *implementation* is the existing JSON `value_from_ref`
  (`json/value.rs:143`); the CSS rider is the isomorphic emission. No vector kernel.
- **Checkasm-analogue (CK).** The **preserve-rich-ast parity differential**:
  dimensions/colors/functions/lists counts reconstructed by the lazy rider must match the
  eager-tree baseline EXACTLY, AND the cssparser 8-field structural equality must hold. This
  is the Checkasm-analogue — a differential against a reference materialization (the eager
  AST + cssparser oracle), the non-vector form of the dav1d scalar-vs-SIMD parity. Per the
  `preserve-rich-ast` non-negotiable, this is a hard gate, not a count nicety.
- **Same-wave-consumer (SWC).** **PRESENT-AND-NAMED = CP-A2 tape sink (its producer).** CP-A3
  is the in-wave consumer of CP-A2's offsets; the producer/consumer pairing is symmetric
  (CP-A2 SWC names CP-A3, CP-A3 SWC names CP-A2). The lazy rider has no purpose without the
  tape it reads, and the tape has no typed-output plane without the rider — the pair is the
  materialization substrate that replaces `emit_fact_stream`.
- **Arch.** Host-neutral.
- **P1 antecedent.** Indirect but profile-grounded: this primitive is what REPLACES
  `emit_fact_stream` (`generated.rs:5`) as the typed-output plane — the fact-stream String
  exists *because* there is no lazy CSS materialization (SYNTHESIS Invalidated ledger: "the
  lazy-view accessor generator does not exist — the gating artefact"). It grounds against
  the SAME hot leaf as CP-A2 (the String floor) by being the materialization plane the tape
  feeds.
- **SOTA antecedent.** simdjson On-Demand lazy iterator (`basics.md:344-350`); sonic-rs
  direct struct deserialization (`README.md:78-90`). The discipline: parse values as used,
  touch source bytes only inside the primitive's own decode.

### CP-A4 — Tokenize-once shared-scan reuse (REDRESS-53-bounded)

- **Shape.** Eliminate the redundant 2–3× re-walk of each declaration body by the SAME
  byte-membership primitive. P1-D §2.5 / `HARDENING-S-P1-V4 §3.3`: each body is scanned by
  `parse_block_item:211 b"{};"` → `find_colon_before:314 b":{};"` →
  `parse_declaration:247 b";}"` — three passes of the same inner loop over the same bytes.
  The candidate is to scan ONCE into the structural index (CP-A1) and have
  declaration/selector parsing consume the index, not re-scan source. Bounded to the
  single-substrate shape (the structural projection IS the tape, Lock 1).
- **Scalar-ref status (SR).** **N/A — a control-flow/consumption change**, not a kernel.
  The SIMD leaf it consumes (CP-A1) carries the vector scalar reference + checkasm.
- **Checkasm-analogue (CK).** The **cssparser 8-field structural equality** — scanning once
  and consuming the index must reproduce rules=10136 / style=9561 / sel=9561 / decls=20043
  EXACTLY (the 2–3× re-walk and the once-scan must be observationally identical). This is the
  Checkasm-analogue: a differential against the multi-pass reference parse.
- **Same-wave-consumer (SWC).** **PRESENT-AND-NAMED = the declaration/selector parse over the
  shared index (CP-A4 IS its own consumer-of-CP-A1).** CP-A4 is the consumer half of CP-A1's
  producer; equivalently CP-A1's SWC names CP-A4 and CP-A4 consumes CP-A1's `Vec<u32>` index.
  There is no orphan scan — the index is produced by CP-A1 and read by the per-grammar
  declaration/selector parse in the same wave.
- **Arch.** Host-neutral (it changes which bytes are read, not how a vector classifies).
- **P1 antecedent.** `find_component_delim` :288 + `find_colon_before` :314 +
  `parse_declaration` :247 (the 2–3× re-walk, `HARDENING-S-P1-V4 §3.3` explicit note). The
  scan self-time (~69%) is partly this redundancy.
- **SOTA antecedent.** sonic-rs LTO-fused single-driver (§1.2): the structural index is
  consumed once, not re-scanned. simdjson Stage 2 consumes the Stage-1 index without
  re-scanning whitespace/structurals.

### Non-candidate (recorded for CH1/CH6 — what the SOTA teardown does NOT yield)

- **CP-NONE: FNV/hex diagnostic.** `push_ascii_lower_hex` (`generated.rs:628`, 8.98–9.11%)
  is a FNV64→lowercase-hex serialization of a source-hash diagnostic field. It has NO
  CSS-semantic value and vanishes wholesale with tape activation (`HARDENING-S-P1-V4 §3.3`:
  "NONE — must NOT be carried into S-P2 as a primitive; FNV bench-only"). Recorded as a
  non-candidate so CH6 cannot read its 9% self-time as an un-addressed leaf — it is
  *retired*, not optimized.
- **CP-BLOCKED: digit/udot kernel.** The udot/i8mm `parse_4_digits_dotprod`
  (`digit_mac.rs:27`, C4b) has ZERO benched CSS antecedent on either current plane (P1-E
  §2.5/anomaly 4(a): recognition counts, it does not decode dimensions). It is
  **orphan-blocked** — NOT a P2-A candidate now. Re-admission condition (verbatim from P1):
  re-profile the typed lazy-`ValueRef` path AFTER the tape lands; admit only if the digit
  leaf is then a top-N tailwind self-time leaf. S-P2 must NOT inherit a CSS digit-kernel
  hypothesis (profile-first non-negotiable). No simdjson/sonic-rs number-leaf candidate is
  promoted here for the same reason (§1.2): no CSS P1 antecedent.
- **CP-BLOCKED: asmjson collapsed-stage FSM.** Host-blocked (x86 AVX-512 only); aarch64
  mechanically refused (`ARCHITECTURE.md:1206`). NOT a candidate (§1.4).

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | Grammar-neutral expression | Verdict | Witness |
|---|---|---|---|
| **CP-A1** byte-class classifier | Pure byte-set classifier. CSS rides `byte_class_from_eq_set_64_neon(src, set: &[u8])` — the set is the grammar's structural-byte set (CSS `b";{}():"`, ≤8 members), supplied per-grammar; the kernel knows no grammar role and the eq-set fan admits ANY alphabet (no lo6 collision constraint). JSON rides the lo6 `vqtbl4q_u8` table route (its `{}[],:"` alphabet is collision-free). | **GRAMMAR-NEUTRAL.** Shared *interface* (`select_classifier` / `PrimitiveKernels`, `dispatch.rs:42`), per-grammar *backend* (CSS = eq-set fan, JSON = lo6 table). Produces only a `Vec<u32>` structural index (no JSON/CSS policy). CSS is the non-JSON exercise per the SYNTHESIS NEON gate / `simd_non_json_exercise=css_l4`. | `dispatch.rs:42`, `byte_class_from_eq_set_64.rs:33`, `json/scan.rs:217`, SYNTHESIS NEON gate |
| **CP-A2** tape append | The `TapeBuilder` is a single non-generic offset/payload sink with no grammar-specific fields (`assembler.rs:42`). JSON rides it today; CSS rides the identical `push_plain_offset`. | **GRAMMAR-NEUTRAL.** Lock 1 substrate-union: ONE substrate, no second tape. The offsets it appends are grammar-agnostic positions. | `assembler.rs:42,71`, SYNTHESIS tape-activation gate |
| **CP-A3** lazy `ValueRef` rider | The cursor walk (`value_from_ref`) is grammar-shaped by the `BackendRule` data the generator walks, NOT by hand-coded per-rule branches. JSON's `value_from_ref` and the CSS rider are the SAME generator over different `BackendRule` shapes. | **GRAMMAR-NEUTRAL by construction (witnessed JSON+CSS only).** Per SYNTHESIS §0.4 generality clause: the exercised riders are JSON (real `value_from_ref`) + CSS (new rich rider). Sheets/BBNF-self generality is asserted-by-construction, proof deferred to SK-V18 (`sheets_witness` has no `BackendRule` to walk). `projection_generality_exercise ∈ {json, css_l4}`. | `json/value.rs:143`, SYNTHESIS Lazy-view receiver |
| **CP-A4** tokenize-once | Reuse of the structural index (CP-A1, grammar-neutral) by the per-grammar parser. The reuse pattern is generic; which bytes index is grammar-specific data. | **GRAMMAR-NEUTRAL** (the index is shared; the consumption is per-grammar template, Lock 14 phrase #1: a generic primitive consumed by a per-grammar template). Bounded to REDRESS-53 single-substrate shape, no parser-local second cursor. | `HARDENING-S-P1-V4 §3.3` REDRESS-53 note |

No P2-A candidate is JSON-overfit. CP-A1 is grammar-neutral by shared-interface/per-grammar-backend
(CSS eq-set fan, JSON lo6 table); CP-A2 is already JSON-witnessed; CP-A3 is JSON-witnessed
and CSS-first-mover; CP-A4 is a consumption pattern over the neutral index. The CP-BLOCKED
items (FNV, digit, asmjson) are dropped, not re-framed.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- **AZ-IV eager-value-tree (SYNTHESIS §0.4; outcome K).** CP-A3 must stay
  lazy-by-default: reconstruct on demand via `ValueRef`, NO eager per-leaf `Box::new`, NO
  f64-alloc-per-number, NO per-color `Box<CssColor>`. The yyjson teardown (§1.3) is cited
  precisely because it shows the DOM can be a contiguous array (the tape), not an eager
  tree. CP-A3 re-opens AZ-IV if it materializes eagerly — REJECT.
- **StructRegistry / Arena<G> / Builder<G> hot-path indirection (SYNTHESIS §0.4).** CP-A2's
  `TapeBuilder` stays a single non-generic sink (`assembler.rs:42`). No registry lookup in
  the per-leaf hot path. CP-A2/A3 re-open this if they introduce a `StructLayout` /
  `TapeStructBuilder` / `TapeCursor` second substrate (Lock 1 type-ambivalence) — REJECT;
  the projection generator emits accessors over the EXISTING `Tape`/`ValueRef`.
- **CSS fact-stream String as admission output (SYNTHESIS §0.4 pre-block-3).**
  `emit_fact_stream` is diagnostic-only; CP-A2 RETIRES it as the live plane. No candidate
  may keep the String as an admission surface.
- **`W5C_REQUEST_FACT_PROFILES` hand-coded routing (SYNTHESIS §0.4; Lock 14 phrase #1).**
  No candidate relocates per-rule branching into projection DATA as an overfit re-entry;
  CP-A3/A4 routing must NAME the `.bbnf` rule it derives from.
- **Unearned-SIMD scan on the lo6/table route (CP-A1 demotion, this cycle).** CP-A1 must NOT
  route CSS through the lo6 `vqtbl4q_u8`/`byte_class_from_table_64_neon` path: that route is
  (a) INADMISSIBLE for every CSS alphabet (the `& 0x3f` low-6-bit guard rejects `;`/`{`
  slot-59 collision, `dispatch.rs:101`) and (b) a scalar passthrough today
  (`aarch64/byte_class_from_table_64.rs:1-4` tail-calls the scalar). Routing CSS there would
  let S-P3 shortlist a SIMD win that silently runs scalar — REJECT. CSS = eq-set fan only.
- **REDRESS 28+33 Class-A tiny-string NEON wiring (`REDRESS.md:325,330,402,631`).** CP-A1
  is a 64-byte block structural classifier, NOT a 16-byte tiny-string dispatch — it must
  not re-introduce the rejected tiny-string kernel into the hot path. (The `match_tiny_plain_string`
  Class-A kernel is REDRESS-blocked; CP-A1 is the Class-B/structural classifier lineage.)
- **REDRESS 82-84 (StringBlock16 tiny probe, single-quartet unicode classifier, object-pair
  compaction; `REDRESS.md:2318`).** CP-A1 must not narrow to a per-grammar single-quartet
  probe; it is the generic `select_classifier` 64-byte route (eq-set fan for CSS).
- **REDRESS 88 (PMULL prefix-XOR as hot body), 89 (CSSC CTZ next-bit bulk consumer).** CP-A1
  uses `bitmap_prefix_xor_64`/`bitmap_next_set_bit` as transient producers inside the
  classifier, NOT as a retained hot body or a cross-call bulk consumer (Lock 1 v+1:
  cross-call classifier-state retention is REJECT).
- **The 24-row evidence-broadcast pre-block (SYNTHESIS §0.2/§0.4).** No candidate's
  *measurement evidence* may be one timing tuple broadcast across N rows; every CSS row is a
  per-corpus N≥50 median (the §1.0 LOCKED V4 band is per-corpus, the run-spread is disclosed
  separately). This is the evidence-measurement broadcast, not a SIMD broadcast op.
- **x86/AVX/SVE (SYNTHESIS §0.4).** asmjson and all AVX-512 routes are host-blocked. CP-A1
  is aarch64 NEON only; no candidate proposes an x86 path.
- **NEON-before-tape inversion (P1-E anomaly 3; SYNTHESIS NEON gate).** CP-A1 is GATED
  behind tape activation — there is no structural index to pre-scan into until the tape
  decodes CSS, and on the typed plane the scan is masked by the String floor. The lever
  order is CP-A2 (tape) → CP-A3 (lazy rider) → CP-A1 (NEON on surviving scan). A candidate
  ordering that lands NEON first re-opens the inverted-lever risk — S-P3 must sequence
  tape-first. **Re-confirm obligation:** the ~69% recognition-scan share is a
  recognition-plane measurement; on the typed plane the scan is masked by the String floor,
  so CP-A1's actual hot-leaf share must be RE-PROFILED after CP-A1/A2 (the tape) land — the
  ~69% is the recognition-plane antecedent, not a measured post-tape scan share. (Aligns with
  the post-CF-1 typed-tape re-profile framing P2-D/P2-F adopt; no claim of a measured
  speculative-rollback or post-tape scan leaf.)

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

External SOTA sources (pinned SHAs, via totality 2A register `2A-sota-landscape.md:113-129`):

- **simdjson** — Langdale & Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019,
  arXiv:1902.08318. `doc/parse_many.md:54-57` (Stage 1/2) + `doc/basics.md:344-350`
  (On-Demand lazy) @ `79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5`.
- **sonic-rs** — `README.md:60-66` (targeted SIMD leaves, rejects two-stage) +
  `README.md:78-90` (direct typed deserialization) @ `03545a9530346fe279b674dd496e037d94204bc5`.
- **yyjson** — `README.md:10-18` (ANSI C, no explicit SIMD), `README.md:73-78`,
  `src/yyjson.h:736-744` @ `d60852703c0fab67d488a692c50ed67d18b467ef`.
- **asmjson** (host-blocked teardown only) — AVX-512 collapsed-stage; instruction footprint
  `MASTER-PLAN.md:702`; aarch64-refused `ARCHITECTURE.md:1206`; PC-as-state non-portability
  `ARCHITECTURE.md:1284`; SOTA-BEAT-DESIGN.md §5.1.
- **lightningcss** — `README.md:10-12`, `src/stylesheet.rs:74-91,122-207`,
  `src/properties/mod.rs:1-18,81-89` @ `ec165294750bb02903e7f845b66533b0465debcc`.
- **cssparser** — `src/lib.rs:12-28`, `src/parser.rs:256-264,695-701,780-788,1122-1151`,
  `src/rules_and_declarations.rs:20-56,196-234,321-358,404-453` @ `4c49486494fb24dc01390e3baca9698ef1744c71`.

ISA / primitive references:

- `byte_class_from_eq_set_64_neon` (the CSS route) — `vceqq_u8` per-member equality +
  `vorrq_u8` OR-reduce + `movemask_u8x16` pack (`bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33-70`);
  ARM NEON intrinsics reference (compare / bitwise-OR family).
- `vqtbl4q_u8` 4-table 64-byte classify (the JSON-only lo6 route) — Lemire 2019 "Arbitrary
  byte-to-byte maps using ARM NEON" (`MASTER-PLAN.md:693`); ARM NEON intrinsics reference
  (TBL/TBX family). Gated by `lo6_table_admissible` `& 0x3f` low-6-bit guard
  (`dispatch.rs:101`); INADMISSIBLE for CSS (`;`/`{` slot-59 collision).
- Validark interleaved movemask (`vshrn`/`vsri`/`zip1`) — validark.dev/posts/interleaved-vectors-on-arm/;
  simdjson PR #2333 (`MASTER-PLAN.md:698`).

bbnf benched-tree sources (verified this cycle at master HEAD `0ae1caa52`, bracket `1c5bd7a25`):

- Recognition path: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
  (`find_component_delim` :288, hot :295/:298; `consume_balanced_at` :320;
  `find_colon_before` :314; `parse_declaration` :247; `parse_block_item` :211;
  `emit_full_parse` :61; summary :91-99; `>=0x80` name byte :404).
- Typed fact-stream path: `generated.rs` (`emit_fact_stream` :5; `push_ascii_lower_hex`
  :628; `fnv64` :619).
- Tape substrate (unwired for CSS): `skinny/crates/runtime/src/tape/`
  (`assembler.rs` `TapeBuilder` :42, `push_offset` :63, `push_plain_offset` :71;
  `mod.rs` `Tape` :94, `ValueRef` :175, `PayloadArena` :38, `DocumentView` :227).
- JSON antecedents: `skinny/crates/runtime/src/grammars/json/scan.rs` (scan fn `:207`;
  `classify_structural_terminator_block_from_table` call `:217-218`;
  `prefix_xor_64`/`escape_mask_64` `:237-239`); `json/value.rs:143` (`value_from_ref` lazy
  projection).
- NEON dispatch: `skinny/crates/bbnf-simd/src/dispatch.rs` (`select_classifier` :42,
  `PrimitiveKernels` + NEON/scalar impls :50-80, `lo6_table_admissible` `& 0x3f` guard :101);
  CSS route kernel `aarch64/byte_class_from_eq_set_64.rs:33`; lo6 table NEON passthrough
  `aarch64/byte_class_from_table_64.rs:1-4`; orphan `aarch64/digit_mac.rs:27`.
- Benched comparator probe: `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`
  (cssparser probe :282-403; lightningcss workload :113; `assert!(n>=50)` :250).

Prior-tranche / pass sources:

- S-P1 LOCKED: `restart/skinny/tranches/sk-v17/research/p1/p1e-hot-leaf-attribution.md`;
  `.../hardening/HARDENING-S-P1-V4-CONSOLIDATED.md §3.1/§3.3/§3.4` (the locked profile band:
  full÷lcss 2.01–3.09× BEATS, fact_stream 0.60–0.77×, lightningcss 833–1261 Mbps).
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1/§0.2/§0.4/§0.5/§0.6 (contract goalset,
  pre-blocks, telemetry).
- `restart/skinny/tranches/sk-v17/research/alpha/alphaB-competitor-deltas.md` (plane
  taxonomy, fair-bar discipline).
- `restart/audit/totality/p2/2A-sota-landscape.md` (the SOTA source register + pinned
  SHAs + the totality teardown this artefact translates to the benched skinny CSS subject).
- `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md` §2 (structural-index-driven shape),
  §3.1 (bbnf-simd crate shape), §5.2 (two-layer vocabulary).
- `restart/locks/LOCKS.md` Lock 1 (:75, substrate-union; :141-149 v+1 cross-call
  classifier-state REJECT), Lock 14 (:603, grammar-generalisation clause), Lock 16
  (:607, primitive-manifest).
- Host: Apple M5 Max, aarch64-apple-darwin. samply 0.13.1.
