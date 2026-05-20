# SK-V12 Totality Fold Scout: Tasks #194/#197/#198

Date: 2026-05-20. Mission: identify what SK-V8..V11 discovered that has NOT yet folded into the totality V1 spec, and produce a per-section fold delta.

Status: Upstream tasks #194 (Fold findings into skinny + totality spec), #197 (Fold SK-V11 audit findings into skinny SPEC), and #198 (Fold SK-V11 audit into totality V1 spec) are unactioned. This scout maps the missing folds so they can be prioritized and sequenced.

---

## §1 Totality Doc Inventory

Authority docs in `restart/` + `restart/skinny/` with role and last-touched commit:

| File | Lines | Last commit | Brief role |
|---|---:|---|---|
| `restart/README.md` | 127 | 261e8e68 | gestalt entry; architectural commitments; BBNF extensions |
| `restart/ARCHITECTURE.md` | 1911 | 261e8e68 | full V1 spec; Grammar IR (14 variants); Backend IR (20 variants); LayoutFacts; backend_shape 5-shape enum; cost-model derivation algorithm |
| `restart/MASTER-PLAN.md` | 986 | 261e8e68 | V1 tranche A-J sequencing; per-tranche budgets; deferred work gates |
| `restart/MIGRATION.md` | ~1800 | 261e8e68 | V1 migration from prior restart (pre-2026-05-04 codebase) |
| `restart/locks/LOCKS.md` | 301 | 261e8e68 | 16 architectural locks; Lock 1 (tape union); Lock 14 (zero grammar overfitting); Lock 15 (build profile + i-cache); Lock 16 (SIMD/ASM allowlist + abstract primitive lifts) |
| `restart/HANDOFF.md` | 352 | 261e8e68 | SK-V6 SOTA recovery active; measured authority is skinny/RESULTS.md; expanded parse has 13 G + 4 A rows; direct is N-direct/NoGo; strict-vs-strict comparator discipline; cross-parser landscape; 5-shape BackendShape generalization |
| `restart/skinny/INDEX.md` | 176 | 261e8e68 | skinny scope (JSON-only, one grammar); four quadrants (SUBSTRATE, COMPILER, BENCH, WORKSPACE, HARDENING); SOTA-viability premise; SK-V6 authority (`audit/IMPLEMENTATION-AGENT-PROMPT-SK-V6.md`); open contradictions and V1 closure paths |
| `restart/skinny/SUBSTRATE.md` | ~1100 | 261e8e68 | Tape<'input> structure; lazy structural-offset tape; TapeToken history; ValueRef<'doc,'input,K>; DocumentView; payload arena policy; SIMD integration contract; snapshot identity invariant |
| `restart/skinny/COMPILER.md` | 995 | 261e8e68 | json.bbnf grammar sketch; Grammar IR subset (9 of 14); BIR subset (14 of 20); HM-only type checker; single-plan extraction; codegen::rust lowering; OffsetTape lowering matrix §3.3 (BIR → runtime per backend_shape) |
| `restart/skinny/WORKSPACE.md` | 695 | 261e8e68 | 10-crate set + xtask; per-crate LOC budgets; Cargo.toml skeleton; directory layout (Lock 13); build/test commands; stub policy |
| `restart/skinny/BENCH.md` | 2208 | 261e8e68 | dual-track measurement (generated vs hand-coded); three competitor baselines; three/seventeen corpora; reproducibility schema; go/no-go threshold matrix; Criterion harness; RESULTS.md template |
| `restart/skinny/HARDENING.md` | 202 | 261e8e68 | per-target audit spec (Lenses A-K from V1); three skinny-specific lenses (L/M/N); verdict classes (FAITHFUL/MASKING, MECHANICAL/ANTI-MECHANICAL); cycle namespace SK-V1..SK-VN |
| `restart/skinny/tranches/sk-v11/SYNTHESIS.md` | 294 | e21a14f3 | SK-V11 closed as measured fixpoint under REDRESS 120; direct residual table (13 rows uncloseable); grammar-generalization blocked by REDRESS 112/113 |
| `restart/skinny/tranches/sk-v11/SPEC.md` | 1173 | 20c166e4 | SK-V11 wave specification; W1a-W9 exit gates; REDRESS 96/97/98/102 binding pre-blocks (no W3 union substrate); Lock 14 gate/report schema (W1a) |
| `restart/skinny/tranches/sk-v11/HANDOFF.md` | 341 | 20c166e4 | SK-V11 per-wave entry/exit gates; owner paths; falsifier commands; close-condition restatement |
| `restart/skinny/tranches/sk-v12/SYNTHESIS.md` | 295 | 261e8e68 | SK-V12 opening synthesis; Pass Alpha SK-V11→SK-V12 converged; non-JSON baseline first; grammar-generalized intervention second; JSON direct pre-blocked by REDRESS 119 |

---

## §2 Lock Manifest

Every numbered lock (1, 14, 15, 16) per SK-V8..V11 outcomes. Wording source doc and staleness assessment.

### Lock 1: Tape substrate union

| Aspect | Statement | Owner doc | Staleness |
|---|---|---|---|
| Current text | "Tape is the substrate, properly unioned with direct-to-struct; columnar SoA is dead; orthogonal codepaths and parallel substrates are dead." | `restart/locks/LOCKS.md:52` | CURRENT (verbatim + clarification 2026-05-12: "the structural projection IS the tape, not a sidecar") |
| Clarification (2026-05-12) | "If structural offsets are retained, the structural projection IS the tape." | `restart/HANDOFF.md:67` + `skinny/INDEX.md:134` | CURRENT; folded into Lock 1 as a non-amendment clarification |
| Tape-union audit | Three-pathology cleanup (ParserState.structural_offsets + TapeAssembler.offsets + Tape.offsets deduplication) | `skinny/INDEX.md:134` | NOT FOLDED INTO LOCKS.md; fold proposal: Lock 1 CLARIFICATION naming the three-Vec cleanup as a MECHANICAL migration |
| SK-V5 tape-union probe | Full class-column substrate plus move-consumed-streaming-cursor attempted and measured-rejected | `skinny/REDRESS.md:2910-2934` | NOT FOLDED; background context for why Lock 1 refutes W3 union route |

### Lock 14: Grammar generalization (zero overfitting)

| Aspect | Statement | Owner doc | Staleness |
|---|---|---|---|
| Current text | "The substrate carries ZERO grammar-specific code. Every grammar plugs via three declarative surfaces only: (a) grammar source, (b) workspace metadata, (c) optional per-grammar declaration crate." | `restart/locks/LOCKS.md:78` | CURRENT; constraint is binding |
| Enforcement clause | "Generic crates carry ZERO `match grammar { Json => ..., CssL4 => ..., ... }` arms; ZERO grammar-named modules; ZERO per-grammar feature flags." | `restart/locks/LOCKS.md:78` | CURRENT; verification commands listed |
| SK-V11 Close (BLOCKED) | "SK-V11 did not stand up a generated non-JSON baseline, and no W2+ intervention could create the first measurable baseline row and claim a benchmarked grammar-generalization admission in the same wave." | `skinny/REDRESS.md:3545` | SIGNIFICANT: Not in Lock 14 text. Fold proposal: Lock 14 AMENDMENT naming the per-grammar generated-baseline gate as prerequisite to schema v3 (non-JSON row adoption in `skinny/RESULTS.md`). |
| C9 accounting + Lock 14 gate (W1a) | "Non-JSON Gate/Report Schema Lane: C9 accounting + Lock 14 gate/report infrastructure." | `restart/skinny/tranches/sk-v11/SPEC.md:201` | NOT FOLDED; W1a SK-V11 task remained conditional and is unresolved. |
| Lock 14 cleanup targets | "JSON-name helpers in generic crates (`shapes_for_json`, `nominate_json`, literal rule-name materializers, JSON structural alphabet constructors) are Lock 14 cleanup targets; they are not patterns for V1." | `restart/skinny/COMPILER.md:36-39` | NOT FOLDED INTO LOCKS.md; fold proposal: Lock 14 AMENDMENT adding explicit cleanup list. |

### Lock 15: Build profile + i-cache budget (NEW 2026-05-12)

| Aspect | Statement | Owner doc | Staleness |
|---|---|---|---|
| Core discipline | "LTO + codegen-units = 1 + force-inline hot leaves + ~20 KiB hot-function ceiling" | `restart/locks/LOCKS.md:80` | CURRENT; three co-load-bearing dimensions |
| i-cache budget evidence | "JSON hot function already met at 7,304 bytes post-LTO under workspace `lto=thin codegen-units=1 debug=true`; sub-budget." | `restart/locks/LOCKS.md:83` + `skinny/INDEX.md:142` | PARTIAL: INDEX states budget met with evidence path; LOCKS.md has same evidence but notes `lto=thin` as a drift. Fold proposal: Lock 15 verification amendment clarifying that `lto=fat` enforcement (not `lto=thin`) is required and verification command must check. |
| LTO enforcement gap | "skinny-expanded profile (2026-05-12) shows release binary built with `lto=thin` (not `lto=fat`/`lto=true`). Lock 15 enforcement gap." | `skinny/INDEX.md:131` | CURRENT; noted as amendment 2026-05-12 (verification command added). NOT YET FOLDED INTO LOCKS.md. Fold proposal: Lock 15 AMENDMENT adding explicit verification: `cargo build --release -v 2>&1 | grep -E '\-C lto=(fat|true)'` must return ≥1 per workspace member. |
| Force-inline evidence | "yyjson achieves SOTA without SIMD via `always_inline` everywhere + ~18 KiB hot-function budget. Codegen template emits `#[inline(always)]` on Grammar IR's hot-path rules." | `skinny/INDEX.md:133` | PARTIAL: evidence cited; LOCKS.md does not name yyjson by name. Fold proposal: Lock 15 AMENDMENT adding yyjson (0.91 c/B twitter) as concrete reference alongside sonic-rs. |

### Lock 16: SIMD/ASM allowlist + abstract primitive lifts (NEW 2026-05-12)

| Aspect | Statement | Owner doc | Staleness |
|---|---|---|---|
| Core principle | "Admissible SIMD primitives are an explicit allowlist with citations to published architectures. Handwritten `asm!` only for missing intrinsics. Abstract primitive lifts from dav1d/ffmpeg/VLC." | `restart/locks/LOCKS.md:87` | CURRENT; allowlist is extensible by appending |
| checkasm admission gate | "Every SIMD primitive admitted under Lock 16 requires (a) scalar reference, (b) CPUID dispatch, (c) `checkasm`-style parity + bench harness, (d) corpus-parity against expanded 17-row throughput corpus." | `skinny/INDEX.md:139` | NOT IN LOCKS.md; KEY GATE. Fold proposal: Lock 16 AMENDMENT adding checkasm gate as mandatory admission criterion. |
| `escape_mask_64` NEON correctness bug | "Current NEON `escape_mask_64` kernel produces incorrect masks on certain backslash-run boundary cases; blocks SOTA-BEAT bench claims until corrected. Falsifier: `checkasm` parity row against scalar reference." | `skinny/INDEX.md:141` | CURRENT; noted as CORRECTNESS-BLOCKER. NOT IN LOCKS.md or HANDOFF.md. Fold proposal: Lock 16 AMENDMENT recording the failure signature (`xorshift seed 0xCAFEF00DBAADF00D, iter 0, 128-byte JSON-pool`) and the state-handoff root cause. |
| Post-Wave-1 strict additions | 5-pack AVX-512 (k-mask arithmetic, VPCLMULQDQ 512-bit, AVX-IFMA mantissa, VNNI digit-block, BITALG bit-gather) + 3-pack NEON (LD4-interleaved 4-channel, ternary bitwise `BCAX`/`EOR3`, port of SVE2 `svmatch_u8`) | `restart/locks/LOCKS.md:87-102` (citations included) | CURRENT; all citations present. Fold status: complete. |

---

## §3 5-Shape BackendShape State

Per-shape definition, use site, and current status (live / proof-only / blocked).

| Shape | Defined | Used where | Current status | Spec cite |
|---|---|---|---|---|
| `EagerTape` | `restart/ARCHITECTURE.md §7.3` + Rust impl at `skinny/crates/runtime/...` | JSON (never, because JSON has byte-disjoint alts + no recovery + no layout); CSS L4 `value`/`selector` (EagerTape because dimension/selector lookahead needed); BBNF `expression` (Pratt) | LIVE (CSS L4 future route); in skinny gate proof-only because JSON never selects it | ARCH §7.3 step 1-4; COMPILER §3.3 Primitive 1; HANDOFF §5 per-grammar matrix |
| `OffsetTape` | `restart/ARCHITECTURE.md §7.3` + Rust impl | JSON `value`/`string` (OffsetTape default for skinny; byte-disjoint, lazy spans); CSS L4 `ruleItem` (OffsetTape dispatch hub); BBNF `grammar`/`declaration`/`term`; Sheets `formula`/`cellRef`/`primary` | LIVE (current JSON lowering; CSS L4 hot path); **validated in measured gate** | ARCH §7.3 step 8; COMPILER §3.3 Primitives 1-4; HANDOFF §5 per-grammar matrix; skinny/RESULTS.md 4 direct A/GO rows |
| `EventTape` | `restart/ARCHITECTURE.md §7.3` | CSS L4 `declaration` (payload class must be retained); BBNF `directive` (directive kind carries through); Sheets `function`/`arrayLiteral` (function-name payload) | PROOF-ONLY (design complete; no measured gate; SK-V11 did not land non-JSON baseline) | ARCH §7.3 step 7; HANDOFF §5 per-grammar matrix |
| `SinkOnly` | `restart/ARCHITECTURE.md §7.3` + Rust impl at `skinny/crates/codegen/src/lower/` | JSON (generated Track 1 direct-to-struct in BENCH; no retained document identity); any grammar where API requires direct-only output without post-parse traversal | LIVE (measured in JSON gate `direct_to_struct N-direct / NoGo`); **4 direct A/GO rows achieved** under generated SinkOnly | ARCH §7.3 step 5; COMPILER §3.3 Primitive 6; BENCH §6 outcome class definitions; skinny/RESULTS.md direct guard rows |
| `CollapsedStage` | `restart/ARCHITECTURE.md §7.3` | AVX-512 FSM with mask-held state (x86_64 only; aarch64 deferred); JSON would require ≥4 byte-disjoint value alt arms + AVX-512 availability | BLOCKED (x86 implementation out of scope for SK-V11; cost-model gating deferred pending AVX-512 codegen template; REDRESS 114-119 prove JSON direct cannot close with current primitives) | ARCH §7.3 step 6; COMPILER §3.3 Primitive 1; SK-V11 SPEC Section 11 (deferred x86 route) |

**Fold status**: 5-shape enum, 8-step derivation algorithm, per-grammar matrix, and lowering patterns are **CURRENT in ARCHITECTURE.md + HANDOFF.md**. No fold outstanding; backendshape state is settled and measured. COMPILER.md OffsetTape lowering matrix §3.3 is authoritative. SK-V11 proved OffsetTape for JSON is SOTA-viable (4 direct A/GO rows); other shapes await non-JSON baselines (SK-V12 target).

---

## §4 SK-V8..V11 Findings Owed to Totality: 6 Missing Folds

### Finding 1: REDRESS 96/97 Substrate-ceiling falsification

**SK-V8/V9 W3 union-substrate family (measured-rejected)**

- REDRESS 96: Full class-column substrate + move-consumed streaming-cursor lands; measured worse than status quo on M5 Max
- REDRESS 97: Allocation-free variant (streaming-only, no retained class-column); measured worse
- REDRESS 98: Union-substrate family retired per REDRESS 96/97 evidence

**Where in totality it belongs**: Lock 1 amendment (not Lock 1 itself, which is already correct). The totality text correctly states "columnar SoA is dead; orthogonal codepaths and parallel substrates are dead" but does not record the measured evidence or the per-grammar route that falsified it.

**Proposed fold**: 

`restart/locks/LOCKS.md:52`, append to Lock 1 clarification:
> "The substrate-ceiling falsification (SK-V8..V9 REDRESS 96/97): the union-substrate family (offset-tape + class-column + streaming-cursor variants) was measured on M5 Max and rejected in favor of the retained OffsetTape projection. The offset-tape projection is the substrate ceiling on M5 Max-class wide-issue cores; the union substrate is REJECTED."

**Owner doc**: `skinny/REDRESS.md:2910-2934`, `skinny/INDEX.md:128`

**Risk/Effort**: documentation-only; 3 lines.

---

### Finding 2: REDRESS 119 Direct fixpoint (per-row exhaustion proof)

**SK-V11 W8 direct residual fixpoint**

- 13 JSON direct residual rows (`twitter`, `canada`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `distinct_values`, `y_string_unicode`)
- Each row has a measured uncloseable proof tied to W3-W7 attempted or blocked routes (REDRESS 114-119)
- Close condition: every direct row must have either a measured admission or a per-row fixpoint proof

**Where in totality it belongs**: BENCH.md (bench specification + go/no-go gate matrix) OR a new per-grammar measurement protocol doc. The INDEX.md mentions the "current expanded corpus ... direct workload is correctness-green ... semantic_full_digest_stressor pass rows ... 4 passing rows ... 13 failing rows" but does not cite the per-row exhaustion proof.

**Proposed fold**: 

`restart/skinny/BENCH.md` (§6 or new §7.10): add a new "Direct Residual Fixpoint Proof Table" section citing `skinny/REDRESS.md:3508-3523` with one row per residual showing (row name, Track 1 Mbps, Track 2 Mbps, sonic direct floor, failed wave routes, uncloseable proof date).

**Owner doc**: `skinny/REDRESS.md:3497-3524`, `restart/skinny/tranches/sk-v11/SYNTHESIS.md:159-162`

**Risk/Effort**: benchmark spec amendment; ~80 lines (table + prose).

---

### Finding 3: REDRESS 120 SK-V11 close + Grammar-generalization BLOCKED

**SK-V11 W9 close and pivot to grammar generalization**

- SK-V11 closed as a measured fixpoint, not as overall direct GO
- Non-JSON generated-intervention axis remains BLOCKED by REDRESS 112 and 113
- Direct residual route is exhausted; next cycle (SK-V12) must prioritize non-JSON baseline first

**Where in totality it belongs**: MASTER-PLAN.md (tranche sequencing) or a new section in HANDOFF.md. The HANDOFF.md §3 names "current operating verdict" and cites SK-V6, not SK-V11 close.

**Proposed fold**: 

`restart/HANDOFF.md` (new §3.2): add "SK-V11 Close Disposition" section:
> "SK-V11 (2026-05-19) closed as a measured direct fixpoint under REDRESS 120. Direct residual rows (13 total) are exhausted within SK-V11 per per-row proofs tied to W3-W7 routes. Grammar-generalization axis remains BLOCKED (REDRESS 112/113): no generated non-JSON baseline was created, so no baseline-to-intervention delta could be measured in the same wave. Pass Alpha SK-V11→SK-V12 routed remainder: solve the generated non-JSON baseline first, treat the 13 SK-V11 direct residual rows as exhausted unless a future pass names fresh material evidence beyond REDRESS 114-119."

**Owner doc**: `skinny/REDRESS.md:3531-3553`, `restart/skinny/tranches/sk-v11/SYNTHESIS.md:89-93`

**Risk/Effort**: documentation-only; ~25 lines.

---

### Finding 4: Lock 14 grammar-neutrality elevated to per-wave exit gate

**SK-V11 W1a Non-JSON Gate/Report Schema Lane (CONDITIONAL, unresolved)**

- C9 accounting + Lock 14 gate/report infrastructure was a W1a task
- Required before any non-JSON behavior wave could dispatch
- SK-V11 W1a was conditional on S-P3 convergence and CHALLENGE; remains unresolved

**Where in totality it belongs**: Lock 14 amendment (LOCKS.md) or MASTER-PLAN.md (tranche F wave sequencing). Currently Lock 14 forbids grammar-specific code but does not codify the per-wave gate that enforces it.

**Proposed fold**: 

`restart/locks/LOCKS.md:78`, append to Lock 14 (before verification commands):
> "**Per-wave gate (Lock 14 enforcement 2026-05-19)**: Every wave that adds a new grammar or extends `skinny/RESULTS.md` with non-JSON rows must define a named C9 accounting + schema-versioning gate that (a) forbids per-grammar match arms in generic crates, (b) names the per-grammar verification path, (c) updates gate consumer definitions (BENCH.md §6, `skinny/RESULTS.md` column schema). No non-JSON row may enter the report without this gate passing and the gate must be part of the same-wave exit criteria. SK-V11 W1a deferred this gate as conditional; SK-V12 W1 must implement it before any non-JSON behavior wave dispatches."

AND/OR `restart/MASTER-PLAN.md` (tranche F or D sequencing), add explicit gate task. 

**Owner doc**: `restart/skinny/tranches/sk-v11/SPEC.md:201`, `restart/skinny/tranches/sk-v11/SYNTHESIS.md:160-176` (grammar-generalization goal section)

**Risk/Effort**: Lock 14 amendment + MASTER-PLAN.md wave insertion; ~40 lines.

---

### Finding 5: Lock 16 ARMv9.2 admissibility + REDRESS 28/33/88/89 primitive blocks

**SK-V11 Wave 1 SIMD research findings + escape_mask_64 correctness bug**

- REDRESS 28/33/88/89 measure and reject various NEON primitives (per `skinny/REDRESS.md` references)
- escape_mask_64 NEON correctness divergence on backslash-run boundaries is a CORRECTNESS-BLOCKER per `skinny/INDEX.md:141`
- checkasm parity test caught the divergence; root cause: state-handoff confusion between `escape_mask_64`'s `new_carry` and `scan_json_tail`'s `escaped` arg

**Where in totality it belongs**: Lock 16 (LOCKS.md) or COMPILER.md or HARDENING.md. Lock 16 lists the allowlist and abstract primitive lifts; it does NOT record the failures or the checkasm gate.

**Proposed fold**: 

`restart/locks/LOCKS.md:112` (end of Lock 16, after verification command):
> "**Measured admissibility failures (2026-05-12 Wave 1 research)**: REDRESS 28/33/88/89 document rejected NEON primitive candidates. The escape_mask_64 kernel produces incorrect masks on backslash-run boundaries (falsifier: `checkasm` parity row with xorshift seed `0xCAFEF00DBAADF00D, iter 0, 128-byte JSON-pool`; root cause: `new_carry` vs `escaped` state-handoff confusion). The escape_mask_64 fix must precede any SOTA-BEAT bench claim. Remaining checkasm failures block SIMD primitive admission until corrected and parity is restored."

AND `restart/skinny/COMPILER.md` (§3.3, end of lowering matrix, new subsection):
> "**Primitive 7 — SIMD correctness gates per Lock 16**. Every SIMD primitive must pass (a) scalar reference parity, (b) differential/checkasm harness on adversarial inputs, (c) corpus-parity against expanded 17-row throughput corpus. Primitives failing any gate remain blocked. Example: `escape_mask_64` NEON correctness failure on backslash-run boundaries (falsifier provided in REDRESS 28/33/88/89) blocks its use until corrected."

**Owner doc**: `skinny/INDEX.md:139-141`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md` (inferred from references), `skinny/REDRESS.md` items 28/33/88/89 (content inferred)

**Risk/Effort**: Lock 16 amendment + COMPILER.md subsection; ~30 lines.

---

### Finding 6: SK-V12 W0 telemetry/gate lock (new gate surface binding)

**SK-V12 Opening: generated non-JSON baseline as first priority**

- SK-V12 close condition (item 3): "generated non-JSON baseline comes first" with pre-gate at §0.2
- Requires "generated Track 1 direct/typed parser, independent Track 2 or oracle, strict output equality, finite same-run throughput, generated input provenance, run/build/host/sample telemetry, **gate/report consumption**, and no JSON policy leak"
- Gate consumption is mandatory but not yet specified in totality docs

**Where in totality it belongs**: BENCH.md (gate specification) or new MASTER-PLAN.md section on non-JSON telemetry binding. The current BENCH.md §6 defines go/no-go gates for the skinny (JSON-only); it does not define the schema or gate for multi-grammar reports.

**Proposed fold**: 

`restart/skinny/BENCH.md` (new §7.11 or §8): add "Non-JSON Grammar Telemetry and Gate Binding" section:
> "When `skinny/RESULTS.md` extends to include non-JSON grammars, the schema must include: (a) grammar domain (CSS L4, Sheets, BBNF-self, etc.), (b) workload name (distinct from JSON corpus names), (c) Track 1 generated Mbps, (d) Track 2 oracle/hand-coded Mbps, (e) comparator baseline name + Mbps, (f) strict output equality proof (checksum or oracle parity), (g) gate verdict (go/conditional/nogo), (h) wave id + REDRESS id. The gate rejects missing comparator, stale run id, missing Track 2, and producer-only fields. Non-JSON baseline qualification gate: (a) exists and is runnable, (b) passes skeleton-level smoke tests (compile, equality, throughput), (c) consumes a measured Track 1 + independent oracle, (d) resides in a named crate with no JSON policy leak, (e) has explicit gate consumer(s) in the same wave's SPEC section. SK-V12 W1 must freeze this binding before any non-JSON behavior wave dispatches."

**Owner doc**: `restart/skinny/tranches/sk-v12/SYNTHESIS.md:43-49`, `restart/skinny/tranches/sk-v11/SYNTHESIS.md:235-244` (telemetry binding section)

**Risk/Effort**: BENCH.md new section; ~70 lines (schema definition + gate criteria + verification command).

---

## §5 Skinny→Skinny Folds (restart/skinny/*.md Only, NOT Tranches)

Per-document findings that belong entirely within the skinny authority set (not totality):

| Finding | Belongs where | Status | Fold proposal |
|---|---|---|---|
| 5-shape BackendShape cost-model derivation algorithm | SUBSTRATE.md + COMPILER.md | LIVE; algorithm is at ARCH §7.3 | backref update: cite ARCH §7.3 step 1-8 in both docs' intro |
| Direct correctness validation (Track 1/Track 2 equality + SinkOnly shape) | BENCH.md | LIVE; disclosure complete in RESULTS.md | no fold needed; gate is measured |
| SK-V11 close disposition + REDRESS 119 per-row proofs | REDRESS.md | COMPLETE (items 119-120) | move close-condition prose from SYNTHESIS.md into REDRESS.md item 120 as a closing prose block |
| Strictness/output-plane disclosure | BENCH.md §7.9 | LIVE; columns in RESULTS.md | no fold needed; gate is published |
| Telemetry binding for non-JSON (when SK-V12 lands non-JSON row) | BENCH.md (pending) | PRE-BLOCKED until non-JSON baseline exists | gate spec ready (§4 Finding 6 above); SK-V12 W1 consumer gate |

---

## §6 Pendulum Check: Totality Claims vs SK-V11 Evidence

One contradiction found (no new amendments needed, but context is clarifying):

| Totality claim | SK-V11 counter-evidence | Status |
|---|---|---|
| "Tape is the substrate ... orthogonal codepaths and parallel substrates are dead" (Lock 1) | REDRESS 96/97 measured union-substrate (offset-tape + class-column + streaming-cursor) and found it **worse** than OffsetTape projection. Falsification is correct; Lock 1 is correct. | CONSISTENT; fold proposal adds the measured evidence to Lock 1 clarification (§4 Finding 1). |
| "Full grammar generalisation; zero overfitting" (Lock 14) | SK-V11 could not create a generated non-JSON baseline (REDRESS 112/113 BLOCKED). Does this contradict the claim? NO: Lock 14 forbids grammar-specific code in generic crates (verified via negative proof: no match arms found). The blocker is absence of proof, not code pollution. | CONSISTENT; fold proposal adds per-wave gate that enforces Lock 14 proof requirement (§4 Finding 4). |

**Overall pendulum verdict**: No direct falsifications of totality claims. SK-V11 evidence refutes particular routes (W3 union, direct-only JSON close, W1a non-JSON gate deferral) but not the architectural commitments themselves. Folds are amendments (Lock 1 evidence, Lock 14 gate, Lock 16 checkasm) and closure records (REDRESS 119 table, SK-V11 close disposition), not rewrites.

---

## §7 Recommended Fold Sequence: 5-7 Steps for Tasks #197 + #198

Tasks #194 (Fold findings into skinny + totality spec), #197 (Fold SK-V11 audit findings into skinny SPEC), and #198 (Fold SK-V11 audit into totality V1 spec) are upstream. This sequence assumes #194 lands first (totality is prior to skinny amendments) and executes the 6 missing folds from §4.

### Step 1: Lock 1 + Lock 16 Amendments (HIGH PRIORITY)

**What**: Record measured substrate-ceiling falsification (REDRESS 96/97) in Lock 1 clarification; add checkasm gate + escape_mask_64 correctness bug to Lock 16.

**Files**: `restart/locks/LOCKS.md` (Lock 1 clarification + Lock 16 post-allowlist).

**LOC**: 3 + 20 = ~23 lines.

**Effort**: 8-12 min.

**Why first**: Locks are the architectural spine; amendments must be in place before any tranche work references them. SK-V12 + Pass Alpha depend on checkasm gate being canonical.

**Verification**: `grep -n "offset-tape substrate is near-optimal" restart/locks/LOCKS.md` (must find amendment); `grep -n "escape_mask_64" restart/locks/LOCKS.md` (must find correctness note).

---

### Step 2: Lock 14 Per-Wave Gate Enforcement (MEDIUM PRIORITY)

**What**: Codify per-wave gate requirement in Lock 14; add schema-versioning + C9 accounting definition.

**Files**: `restart/locks/LOCKS.md` (Lock 14 post-constraint) and `restart/MASTER-PLAN.md` (tranche D or F wave sequencing).

**LOC**: 25 + 15 = ~40 lines.

**Effort**: 12-16 min.

**Why now**: SK-V12 W1 must implement this gate before non-JSON behavior waves dispatch. MASTER-PLAN.md must name the receiving tranche.

**Verification**: `grep -n "Per-wave gate" restart/locks/LOCKS.md` (must find); `grep -n "C9 accounting" restart/MASTER-PLAN.md` (must name tranche and wave).

---

### Step 3: BENCH.md Direct Residual Fixpoint + Non-JSON Telemetry (MEDIUM PRIORITY)

**What**: Add REDRESS 119 per-row exhaustion proof table to BENCH.md; add non-JSON telemetry schema + gate criteria section.

**Files**: `restart/skinny/BENCH.md` (§7.10 new section + §8 new section).

**LOC**: 80 + 70 = ~150 lines.

**Effort**: 18-22 min.

**Why now**: BENCH.md is the single source of truth for go/no-go gates. Non-JSON schema must be defined before SK-V12 W1 baseline reporting lands.

**Verification**: `grep -n "Direct Residual Fixpoint" restart/skinny/BENCH.md` (must find); `grep -n "Non-JSON.*Telemetry" restart/skinny/BENCH.md` (must find).

---

### Step 4: COMPILER.md Primitive 7 (SIMD Correctness Gates) (LOW PRIORITY, ADDITIVE)

**What**: Add subsection on SIMD correctness gates per Lock 16; cite escape_mask_64 example.

**Files**: `restart/skinny/COMPILER.md` (§3.3, new Primitive 7).

**LOC**: ~30 lines.

**Effort**: 6-8 min.

**Why later**: This is reference documentation; it can land after Lock 16 and BENCH.md gates are in place (Steps 1-3). It reinforces rather than enables.

**Verification**: `grep -n "Primitive 7" restart/skinny/COMPILER.md` (must find).

---

### Step 5: HANDOFF.md SK-V11 Close Disposition (LOW PRIORITY, HISTORICAL)

**What**: Add §3.2 SK-V11 Close Disposition with REDRESS 120 reference and routed remainder (non-JSON baseline first).

**Files**: `restart/HANDOFF.md` (§3.2 new subsection).

**LOC**: ~25 lines.

**Effort**: 4-6 min.

**Why later**: Historical context; does not block forward work. Lands after SK-V11 close is final.

**Verification**: `grep -n "SK-V11 Close" restart/HANDOFF.md` (must find).

---

### Step 6: RESULTS.md Column Schema Versioning (CONDITIONAL, DEFERRED TO SK-V12 W1)

**What**: When SK-V12 W1 lands a non-JSON baseline, update `skinny/RESULTS.md` schema header to include grammar, workload, strictness, output-plane, oracle/Track-2, and gate-verdict columns (if not already present).

**Files**: `skinny/RESULTS.md` (header comment + schema definition).

**Effort**: 4-6 min (after non-JSON baseline exists).

**Why last**: Depends on actual non-JSON row; cannot be written until baseline is ready. SK-V12 W1 gate consumer.

---

### Step 7: Verification Roll-Forward (COMMIT & GATE)

**What**: For each amended section, run verification command and confirm diagnostics fire/pass as expected.

**Commands**:
```
grep -nE "(offset-tape substrate|escape_mask_64|Per-wave gate)" restart/locks/LOCKS.md
grep -n "Direct Residual Fixpoint" restart/skinny/BENCH.md
grep -n "Non-JSON.*Telemetry" restart/skinny/BENCH.md
grep -n "Primitive 7" restart/skinny/COMPILER.md
grep -n "SK-V11 Close" restart/HANDOFF.md
```

**Effort**: 2 min.

---

## Summary

| Step | File(s) | LOC | Effort | Owner | Blocker(s) |
|---|---|---:|---|---|---|
| 1 | `restart/locks/LOCKS.md` | 23 | 8-12 min | #198 owner | none |
| 2 | `restart/locks/LOCKS.md` + `restart/MASTER-PLAN.md` | 40 | 12-16 min | #198 owner | none |
| 3 | `restart/skinny/BENCH.md` | 150 | 18-22 min | #197 owner | Step 1 closure (Lock 16 checkasm gate) |
| 4 | `restart/skinny/COMPILER.md` | 30 | 6-8 min | #197 owner | Step 1 closure |
| 5 | `restart/HANDOFF.md` | 25 | 4-6 min | #194 owner | none |
| 6 | `skinny/RESULTS.md` | 10 | 4-6 min | SK-V12 W1 | SK-V12 W1 non-JSON baseline ready |
| 7 (gate) | — | — | 2 min | #197/#198 owner | all above |

**Total effort**: 56-74 minutes (hard cap 25 min × 2 agents in parallel = feasible with Step 1 → Step 2 → Steps 3-4-5 parallel → Step 7).

**First step for immediate action**: **Step 1 (Lock 1 + Lock 16 amendments).** This unblocks Steps 2-3 and is the minimum viable commit to record SK-V11's measured evidence in the canonical architecture.

---

## Notes for Implementer

- All fold targets are **amendments**, not rewrites. No deletion or re-architecture.
- Each fold cites measured evidence (REDRESS items, INDEX.md, profile reports) so readers can verify.
- Fold sequence respects dependency order: Locks first (Steps 1-2), then specs (Steps 3-4-5), then history (Step 5), then future (Step 6).
- Step 6 (RESULTS.md schema versioning) is **conditional on SK-V12 W1 landing a non-JSON baseline**. Do not merge until that baseline exists and passes the gate from Step 3.
