# SK-V3 Go / No-Go Synthesis

**Date**: 2026-05-13
**Cap**: 45 min (synthesis only; companion reports already on disk)
**Inputs**:
- `/tmp/psi-excavation-report.md` (187 lines — git history / commit census)
- `/tmp/psi-archaeology-report.md` (492 lines — archives / verbatim quotes)
- `/tmp/psi-failure-anatomy.md` (360 lines — 5+1 named failure modes)
- `/tmp/skv3-psi-diff-audit.md` (125 lines — claim-by-claim architectural diff)
- `/tmp/fsm-correctness-audit.md` (353 lines — FSM/DPDA/RD verdict)

---

## TL;DR — honest verdict

**Dispatch SK-V3 with one binding amendment: quarantine `CollapsedStage` (the FSM-shaped backend) behind an explicit precondition gate and excise its branch from the SK-V3 close criterion.**

The other four `BackendShape` values (`EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`), the two NEON kernels (Class A `tiny_string` + Class B `\uXXXX`), Plan D capacity policy, the `escape_mask_64` correctness fix, and the checkasm differential harness are all VERIFIED-DIFFERENT from PSI/DTA failure shapes and carry their own falsifiability gates. They land.

`CollapsedStage` carries one structural risk no spec edit can dissolve: it is **codegen-emitted FSM derived from grammar shape**, which is the same risk shape as AW-V's "auto-derive the sonic-rs-class inner loop from any BBNF grammar" thesis that demonstrated once on JSON at W3 and was lost by W6. Combined with the fact that asmjson is actually a **9-state DPDA with hardware-bounded explicit stack** (not a pure FSM, per FSM-audit §d), the "outclass asmjson by 1.28× by codegen-emitting a richer DPDA from Grammar IR" framing is the structural successor to the auto-derive thesis.

The fix is NOT to delete `CollapsedStage` from the taxonomy; the cost-model 8-priority derivation is correct and `CollapsedStage` falls out as one of five honest projections. The fix is to **never let `CollapsedStage` block the SK-V3 close** and to **never let `CollapsedStage` ship without Zen 4 silicon access + an admission probe that empirically shows the derivation produces correct output on at least JSON before any second grammar enters the cost-model surface**.

If Zen 4 silicon does not materialise within the SK-V3 cap, `CollapsedStage` defers to a successor tranche with its own plan document. The SK-V3 close gate is satisfied by Phase 1a + Phase 1b + Phase 2 on M5 Max — sub-700 LOC NEON + lowering template, all checkasm-gated, no FSM-derivation work touched.

Verdict: **GO on SK-V3 minus `CollapsedStage`. CONDITIONAL on `CollapsedStage` (separate tranche, separate plan).**

---

## §1 — Lessons learned from PSI's failure (the 5+1 named modes)

The five canonical Lock 1 modes plus one excavation-surfaced mode.

### 1.1 OpenFrame clone parallel substrate (`Vec<OpenFrame>::clone` — 86.07% pathology)

The Era IV/V `OpenFrame` was a speculative-branch checkpoint frame: when a parse rule entered a speculative branch, the builder *deep-cloned its in-flight Vec<OpenFrame>*, attempted the branch, then either committed the clone or discarded on failure. CSS L4 carried **14 variants** of OpenFrame. The clone cost was proportional to `depth × per-frame payload`. Samply measured **86.07% inclusive on `bbnf_value_twitter`** as `Vec<OpenFrame>::clone` from `JsonStructBuilder::checkpoint` (`restart/MIGRATION.md:347`; `docs/GESTALT.md:13`).

Root cause: the tape was *write-forward* with no bounded-rollback primitive; checkpointing was implemented at the *builder* layer above via `Vec::clone`. There was no architectural place to put rollback that did not require cloning the speculative stack.

**Physical residue at 2026-05-12**: OpenFrame still present in `crates/core/src/runtime/builder_template.rs`, `crates/core/src/grammar/generated/{json,bbnf,css_l4,css_pretty,google_sheets}.rs`, `crates/core/src/runtime/google_sheets/arena.rs`. The greenfield retirement is planned but unfinished.

### 1.2 Type ambivalence (tape ↔ OpenFrame ↔ direct-to-struct competing for the same role)

Three representations competing for "what the parse output is":
1. **Tape** (`crates/tape/src/`) — Era IV's `TapeRec` + payload arenas + `FusedBuilder`.
2. **OpenFrame** — per-grammar speculative builder frame; CSS L4 carried 14 variants.
3. **Direct-to-struct** — hand-written `bbnf::json::Value` / `bbnf::css::StyleSheet`; AX.W1.A/W1.B briefly landed, deleted at W1r.0 (`3429aaba`: −6,128 LOC).

No code path could trust one representation without re-deriving from the others. Cost: indirect cloning, double materialisation, dead writes.

### 1.3 Substrate-first / consumer-later (the Era V signature)

Seven substrates shipped (DTA, PSI, columnar tape, ShapeRef, PHF+SIMD classifiers, Bloom+GADT dedup, Shape emitter). At Era V close, **0/17 parse entries** exceeded the AU baseline; CSS / Sheets / BBNF landed at 3-7% of baseline; JSON twitter 486 MB/s (24.7% of AU). Substrates landed without runtime consumers.

The post-mortem produced the canonical orchestrator rule (`LESSONS-LEARNED.md:17-26`, 2026-04-29): *"every substrate change must land with a same-wave consumer or an explicitly declared brittleness window and restoration wave."* AX Proposition 4: *"Novel levers compound only when they share a substrate AND a demonstrable floor."*

### 1.4 Columnar SoA designed but never activated (AV.04)

`docs/tranches/AV/research/04-columnar-soa.md` (178 lines) specified a kind-partitioned columnar store as the entire tape substrate. The activation happened first in Era IV (Tranche Y: 7 structural Vecs as per-kind columns). **AY-I.W1 reverted to single `Vec<TapeRec>` + `sib_skip`** (commit cluster per `era-VI-restart.md:64-71`) because the 7-column AoS lost to cache-locality of a single AoS record. The AV.04 "kind-partitioned" variant never reached runtime.

Lock 1 keeps columnar SoA **buried**.

### 1.5 Per-grammar god-modules in generic crates (Lock 14's enumerated list)

Per `restart/locks/14-LOCKS.md:60`:
- CSS L4 14-variant `OpenFrame`
- BBNF aggregator `pub use bbnf::*`
- Sheets arena fallbacks
- Per-grammar registry arms in `bbnf-ir`
- `shape_dict_bbnf.rs`
- `crates/core/src/css_types.rs`
- Per-grammar `runtime/<g>/` hand-written modules

### 1.6 NEW failure mode surfaced by excavation: the interpreter-dispatch ceiling

Not in Lock 1's verbatim text, but called out explicitly by the excavation: **the DTA's `dispatch_one` carried 20–35% self-time across every grammar and every input size** (`aw3-r6-path-b-rip-dta.md:13-15`): *"No AW-IV lever touches the tagged-union match over 20+ `DtaState` variants; it is the canonical state-machine-interpreter ceiling."* PSI's parallel-fill speedup could not amortise this serial interpreter overhead. The W2.1 prototype (`crates/bbnf-json-prototype/`) proved beat-sonic-rs achievable without PSI/DTA at all — `nm` for `dispatch_one` was empty in the bench binary.

**Generalisation**: any per-rule indirection through a runtime dispatch table — including a 256-entry function-pointer table, including a `[DtaStateId; 256]` LUT, including a `match table.states[N]` over a const table the emitter knows — pays a dispatch ceiling that no SIMD/parallelism lever can amortise. Fix shape: emit the dispatch *into* the generated code as inlined `match`, not through runtime indirection.

This is the failure-mode signature most directly relevant to `CollapsedStage`'s codegen-emitted-FSM proposition.

---

## §2 — SK-V3 risk audit (per PSI failure mode)

Verdict notation per task spec: **VERIFIED-DIFFERENT** / **SAME-CLASS-DIFFERENT-INSTANCE** / **REBRANDING** / **UNDETERMINED**.

| Failure mode | SK-V3 surface | Verdict | Mechanism + Lock |
|---|---|---|---|
| 1.1 OpenFrame clone | Lock 1 verbatim refutation: *"no parallel offset stream"*; rollback is a tape primitive, not a builder primitive. 8-step `derive_backend_shape` step 4 (`ARCHITECTURE.md:1078`): rules with overlapping Alt first-set → `EagerTape`, lowers Alt as `Speculative` with bounded checkpoint on the tape itself, not Vec<OpenFrame>. | **VERIFIED-DIFFERENT** | Lock 1 + `ARCHITECTURE.md:1465` ("Rollback is bounded and does not clone OpenFrame stacks"); verification grep `rg "OpenFrame\|Vec<OpenFrame>\|ParseStream" crates/runtime/src crates/codegen/src` must return zero (`MASTER-PLAN.md:307`). |
| 1.2 Type ambivalence | Lock 1 union: tape ∪ direct-to-struct as ONE substrate with five projections (`EagerTape / OffsetTape / EventTape / SinkOnly / CollapsedStage`). `SinkOnly` is the only direct-only shape; the other four retain `(TapeId, cursor, event_kind_or_payload_class)` identity (ARCH §9). | **SAME-CLASS-DIFFERENT-INSTANCE** | The 5-shape taxonomy *resembles* the EmissionTier 3-shape taxonomy that AQ.5 deleted (`era-IV-tape-first.md:62-70`) as orthogonal-decision-surface. SK-V3 mitigates by housing `backend_shape` in `LayoutFacts` as ONE side-table field with ONE 8-priority derivation algorithm (`ARCHITECTURE.md:1075-1082`). Falsifiable risk: if any of the 5 shapes proves redundant (e.g., `EventTape` collapses to `OffsetTape` once typed event cursor lands), this is AQ.5 recurring. Lock 10 (cost model + shape miner) + falsifiability gate: **the wave dispatcher must record per-grammar derivation choice and the cost model must show ≥2 of the 5 shapes firing across the 9-grammar matrix, or the taxonomy collapses on AQ.5 precedent.** |
| 1.3 Substrate-first / consumer-later | Lock 1 forbids it; `LESSONS-LEARNED.md:17-26` codifies the orchestrator rule. IMPLEMENTATION-PACKET §3-§7 wave structure must exhibit same-wave consumer wiring. | **SAME-CLASS-DIFFERENT-INSTANCE** | Wave 1 (typed event cursor) ships substrate + consumer same wave (NEON Class A/B kernels are wired to specific generated.rs:161-172 + parser.rs:78-113 call sites). Wave 4 (`parse-that` primitives) ships substrate + view accessors same wave. **The risk concentrates on Wave 3** (`bbnf-simd` kernel contract host-aarch64 first) which can drift into substrate-without-direct-consumer if Wave 1's NEON consumers don't already exist. Mitigation: Wave 3 follows Wave 1 in the IMPLEMENTATION-PACKET; Wave 3's primitives are NOT shipped without a Wave 1 call site already consuming them. Verification gate: each wave's exit-gate cites a samply profile rooted in the consumer call site. |
| 1.4 Columnar SoA | Lock 1 explicitly forbids revival. The structural projection IS the tape (offsets + flags + payload arena as ONE structure, three arrays carried together, not column-per-grammar-field SoA). | **VERIFIED-DIFFERENT** | `SUBSTRATE.md:217` verbatim; the three-array shape (offsets + flags + payload arena) is what simdjson, asmjson, yyjson, and sonic-rs all retain. Distinct from AV.04's per-kind SoA. |
| 1.5 Per-grammar god-modules | Lock 14 verification commands: `rg JsonParser\|CssL4Parser\|... crates/{ir,parse,...}` must return zero. `bbnf-simd` carries zero grammar names by trait surface (`SimdClassifier::classify_chunk`). | **VERIFIED-DIFFERENT (provisional)** | The crate is built and gated. Verification command at every CI run. Provisional because cost-model-derived `OffsetTape` for "byte-finite disjoint first-set" produces identical-looking code per grammar — which is the GOAL, not a defect. |
| 1.6 Interpreter-dispatch ceiling (NEW) | `Alt { Dispatch }` lowers to inlined `match b` on `source[offsets[cursor]]` — emitted IN the generated code, not through runtime indirection. The `EmitStrategy::for_grammar` registry holes (CSS-L4 parity test fails per REAUDIT-2026-04-30 §3 row 6: `pipeline_compile_request::*` × 6 panic at `crates/ir/src/registry/strategy.rs:257`) are NOT in the SK-V3 critical path — they are CSP-of-strategy decision holes in tranches A-J, distinct from the parse hot path. | **VERIFIED-DIFFERENT for OffsetTape**; **UNDETERMINED for CollapsedStage** | `OffsetTape` lowering inlines completely; sonic-rs's 91.15% self-time on one monomorphised symbol is the existence proof (per `aw5-r5-depart-rip-dta.md`). `CollapsedStage` proposes `r10` indirect jump — *this is the same shape* as the DTA's `[DtaStateId; 256]` LUT but in registers instead of memory. Whether LLVM compiles the `match next_state` to an indirect branch with predicted target vs a chain of conditional branches is hardware-and-LLVM-version-specific; the M5 Max NEON path does not have `jmp r10`-equivalent at intrinsics layer. The codegen-emitted-FSM concern (skv3-psi-diff-audit §d) is the unresolved residual. |

**Net**: 3 VERIFIED-DIFFERENT, 2 SAME-CLASS-DIFFERENT-INSTANCE (mitigated, not eliminated), 1 UNDETERMINED for `CollapsedStage` specifically. Zero REBRANDING.

---

## §3 — The FSM/DTA verdict

**Q: Should we proceed with the `CollapsedStage` FSM-shaped backend?**

A: **NO — not as part of the SK-V3 close.** YES — as a separate, conditional, post-SK-V3 tranche with its own plan document and a hard precondition gate.

The FSM-audit (`/tmp/fsm-correctness-audit.md` §e) is correct in its narrow claim: a per-rule `CollapsedStage` for grammars that admit it (no `@error(recover)`, no `@host fn` parse-time-decoded, no `@layout`, first-set disjoint, target-feature admissible) is architecturally sound *as a taxonomy entry*. The cost model has the right discriminators. JSON admits all five; CSS L4 fails the first three; CSV is the only other plausible candidate.

But three structural risks compound at SK-V3 dispatch time:

**Risk A (the AW-V recurrence shape)**: `CollapsedStage` is **codegen-emitted FSM (actually DPDA) derived from Grammar IR**. The asmjson reference is a hand-written .S file for one grammar. The qualitative leap from "hand-written FSM for one grammar" to "codegen-emit FSM derived from any Grammar IR via 8-step cost model" is precisely the leap AW-V tried with the shape emitter — "auto-derive the sonic-rs-class inner loop from any BBNF grammar" — and lost by W6. The structural parallel:

- AW-V: auto-derive sonic-rs-class RD from BBNF → JSON only at W3, lost by W6
- SK-V3 Phase 4: auto-derive asmjson-class 9-state DPDA from BBNF → JSON only proposed, gated on Zen 4 access

**Risk B (the asmjson-is-actually-DPDA shape)**: per FSM-audit §d, asmjson is **not a pure FSM** — it carries `frames_buf[MAX_JSON_DEPTH=64]` + `open_buf[64]` as a hardware-bounded explicit stack. The "9-state FSM" framing in `SOTA-BEAT-DESIGN.md` §5.1 is technically the finite-control fragment only; the actual recognizer is a DPDA. A codegen-emitted version of asmjson must derive the per-grammar stack discipline (bracket-pair set, depth bound, open-token validation) from Grammar IR — and that derivation has a per-grammar state-explosion bound the cost model has not yet been audited for.

**Risk C (the silicon-access shape)**: `CollapsedStage` admission requires AVX-512 VBMI2 — the dev box is M5 Max (arm64); the target silicon for the SOTA-BEAT-asmjson claim is Zen 4. Per `WAVE-1-2-COHORT-DIGEST.md:232-241`, Zen 4 silicon access is currently UNAVAILABLE. Without silicon, the Phase 4 gate (twitter T1 ≥ 7400 MiB/s on x86_64) cannot be measured; the close criterion collapses to "compiles successfully" which is the substrate-first-consumer-later failure-mode signature.

**Q: Or drop `CollapsedStage` and rely on the other 4 shapes?**

A: **Yes — for the SK-V3 close.** Phase 1a (Class A NEON kernel, ~80 LOC) + Phase 1b (Class B NEON kernel, ~70 LOC) + Phase 2 (`LayoutFacts.backend_shape` cost model + `Alt { Dispatch }` two-access-pattern lowering, ~470 LOC) closes the M5 Max expanded-corpus gate (per `SOTA-BEAT-DESIGN.md` §10 verdict) without any FSM derivation work. Sub-700 LOC total. Target: twitter T1 ≥ 2375 MiB/s (BEAT sonic-rs Value-DOM 2438 MiB/s); 0 G-rows on expanded corpus.

**Q: Or restrict `CollapsedStage` to feature-gated x86_64 AVX-512 only?**

A: **Yes — that is its admission contract.** The cost model already gates it on `target_features.has("avx512vbmi2") AND rule is a hub with ≥4 byte-disjoint arms AND no recovery/layout/host-decode body`. The restriction is correct; the residual risk is dispatch timing: don't dispatch `CollapsedStage` work before silicon is available, OR dispatch it as an asynchronous research lane behind the SK-V3 critical path.

**Q: Or acknowledge that codegen-emitted FSM is a fundamentally different problem from hand-written ASM FSM (asmjson) and approach it differently?**

A: **Yes — this is the core insight.** Three concrete corollaries:

1. **The asmjson "FSM" is a DPDA + direct-threaded dispatch + hand-curated per-state classifier masks.** Codegen-emit'ing all three from Grammar IR is *not* the same problem as porting the runtime walker. The per-grammar stack discipline must be derived; the per-state classifier masks must be derived from per-state first-sets; the direct-threaded dispatch must compile to indirect branches LLVM actually predicts on the target uarch. None of these have been demonstrated for any grammar.

2. **Phase 4's projection (14.0 GiB/s = 1.28× asmjson) is conditional on cost-model derivation producing correctness-equivalent output to the hand-written .S file.** No checkasm-style differential harness exists for FSM derivation parity (the existing checkasm gates SIMD primitives, not state-machine codegen). If `CollapsedStage` ships, **a new differential parity harness is needed**: cost-model-derived FSM output vs hand-written reference (asmjson on Zen 4; smaller reference on M5 Max if any). Absent the harness, the substrate ships without a correctness consumer — Era V signature.

3. **The right approach is "DPDA codegen as a small new research crate"** that lives parallel to `bbnf-simd`, has its own scalar reference (the existing `OffsetTape` RD code is the reference), and graduates into the cost-model surface only after it passes a per-grammar parity harness. This is NOT in SK-V3's scope; it is a successor tranche with its own plan document.

**Final FSM verdict**: `CollapsedStage` is correct as a taxonomy entry, correct in its admission predicates, but **must not be dispatched as part of the SK-V3 close**. The SK-V3 close is satisfied by `OffsetTape` + the two NEON kernels.

---

## §4 — Concrete spec amendments needed

### 4.1 If `CollapsedStage` is dropped from the SK-V3 close (recommended)

Edit `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`:

**Amendment 1 (Non-negotiable, after line 37)**:

> Phase 4 (`CollapsedStage` AVX-512 backend) is NOT part of the SK-V3 close criterion. The SK-V3 close requires the expanded SOTA-BEAT gate on M5 Max (arm64); it does not require any x86_64 path. If Zen 4 silicon access does not materialise within the SK-V3 cap, Phase 4 (Wave 6 in this packet) is deferred to a successor tranche with its own plan document, NOT folded into a successor wave of SK-V3. This guards against the AW-V auto-derive failure-mode recurrence (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:187-188`).

**Amendment 2 (Wave 6 §8 preamble rewrite)**:

> Wave 6 (x86_64 strict SOTA path) is conditional. It ships in two sub-waves:
> - Wave 6a (Phase 3, AVX-512 VBMI2 primitives in `bbnf-simd`): unconditional once x86_64 dev access exists. Lands the GFNI / k-mask / VPCLMULQDQ / AVX-IFMA / VNNI / BITALG primitives as primitives (Lock 16 5-pack), each with a scalar reference + checkasm parity. **These are admitted independently as primitives consumed by the existing `OffsetTape` and `EventTape` shapes on x86_64.** No new backend shape required.
> - Wave 6b (Phase 4, `CollapsedStage` backend): **deferred to a successor tranche.** Requires (1) Zen 4 silicon access for empirical Phase 4 gate measurement; (2) a new differential parity harness comparing cost-model-derived FSM output against the asmjson reference on at least JSON; (3) cost-model-firing audit showing `derive_backend_shape` returns `CollapsedStage` for the JSON dispatch hub and `OffsetTape` for CSS L4 / BBNF-self / Sheets dispatch hubs on the same shape-miner pass. Without all three, dispatching Wave 6b risks the Era V substrate-first-consumer-later signature.

**Amendment 3 (close-condition update at §0)**:

> Add: "The expanded corpus close is satisfied on the M5 Max host without any `CollapsedStage` participation. x86_64 native sidecar rows are reported as reference comparators; they do not require BBNF-generated `CollapsedStage` code to publish."

**Amendment 4 (rename or footnote "FSM")** per FSM-audit §e:

In `restart/skinny/audit/SOTA-BEAT-DESIGN.md` §5.1, change the heading "9-state FSM and PC-as-state" to **"9-state DPDA: 9-state finite control, direct-threaded dispatch via `r10`, hardware-bounded explicit stack (`open_buf[MAX_JSON_DEPTH=64]`) for container nesting"**. This is documentation, not architecture.

### 4.2 If `CollapsedStage` is kept in the SK-V3 close (NOT recommended)

Risk mitigations that MUST land before any Phase 4 wave dispatches:

1. **Differential parity harness for FSM codegen** (analogue to `bbnf-simd/tests/checkasm_parity.rs`): `tests/fsm_codegen_parity.rs` running cost-model-derived FSM bytecode against a hand-written reference for at least JSON. Existence gate before Phase 4 wave dispatch.
2. **`derive_backend_shape` firing matrix audit**: enumerate per-rule shape selection across JSON, CSS L4, BBNF-self, Sheets, CSV, EBNF, BNF, math; record verdict. If `CollapsedStage` fires only on JSON, fold the diagnostics into Lock 14 as a per-grammar god-module signature.
3. **Zen 4 silicon access requirement** declared explicitly in `WORKSPACE.md` profile section.
4. **AVX-512 5-pack admission as primitives separable from `CollapsedStage`**: GFNI / k-mask / VPCLMULQDQ-512 / AVX-IFMA / VNNI / BITALG each land as standalone `bbnf-simd` primitives consumed by `OffsetTape` on x86_64 first; only after they pass checkasm parity AND consumer wiring does `CollapsedStage` get to admit them in fused form.

### 4.3 If `CollapsedStage` is gated (the precise gate)

The gate must be all of:
- Cost model: `target_features.has("avx512vbmi2") AND rule_is_hub AND ≥4_byte_disjoint_arms AND no_error_recover AND no_host_fn_parse_time AND no_layout_scope`
- Tooling: `BBNF_SIMD_STRICT=1 cargo test -p bbnf-runtime --release --test fsm_codegen_parity` returns 0 divergences
- Empirical: Phase 4 throughput gate (twitter T1 ≥ 7400 MiB/s, hot-leaf count = 1, c/B ≤ 0.45) measured on Zen 4 silicon
- Per-grammar: `derive_backend_shape` returns `CollapsedStage` for ≥1 non-JSON grammar OR the per-grammar god-module audit records "JSON-only" as a known acceptable scope

All four gates must hold concurrently before any `CollapsedStage` runtime call site fires in a release build.

---

## §5 — What to keep from SK-V3 unchanged

These items are validated by the excavation, the failure-mode anatomy, and the diff audit; none are at PSI-recurrence risk.

| Item | Why keep | Citation |
|---|---|---|
| Class A NEON kernel (16-byte `match_tiny_plain_string` via `vqtbl4q_u8` + `vshrn` movemask) | Kernel-level fix to specific scalar-loop pathology at `generated.rs:161-172`; closes Class A corpora (github_events, update-center, random); checkasm-gated; ~80 LOC | `SOTA-BEAT-DESIGN.md` §3.2.1; `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §3 Wave 1 |
| Class B NEON kernel (TBL-driven `\uXXXX` hex decode via `vqtbl1q_u8`) | Kernel-level fix to scalar nibble cluster at `parse_that_regex::unescape_json_string`; closes Class B corpora (unicode_escapes, y_string_unicode); checkasm-gated; ~70 LOC | `SOTA-BEAT-DESIGN.md` §3.2.2 |
| Plan D capacity policy (`Vec::with_capacity(256)` + geometric grow) | Empirically validated (Wave 2 Agent 6): +4.8% random, +10.2% github_events; 23-64% capacity reclamation; sampled heuristic over-reserves 2.53×, Plan D lands at 1.87× | Wave 2 capacity report; `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` P0.1 |
| checkasm differential harness (`crates/bbnf-simd/tests/checkasm_parity.rs`, 516 LOC) | dav1d/FFmpeg discipline transfer; caught `escape_mask_64` bug on first run; admission gate has teeth | `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §3 admission gate; FFmpeg `checkasm.h` lineage |
| `escape_mask_64` NEON correctness fix (state-handoff bug; adversarial repro xorshift seed `0xCAFEF00DBAADF00D`) | Blocks SOTA-BEAT bench claims; required before any wave-1 bench publication; ~30 LOC | `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` P0.2 |
| 4 of 5 `BackendShape` taxonomy values: `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly` | Cost-model-derived per-rule; 8-priority derivation algorithm; no new directive; no new BIR variant. Distinct from EmissionTier (one side-table field, not two). | `ARCHITECTURE.md` §7.3; `SUBSTRATE.md:213-219` |
| Lock 15 build profile (`lto="fat"`, `codegen-units=1`, `debug=true`) | Wave 2 PMU confirms fused `parse_value_at` is 7,304 bytes ≪ 20 KiB i-cache budget; sonic-rs NOINLINE wall-clock falls 2.1-3.2× without it; co-load-bearing with codegen template | `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` P0.3; sonic-rs-v2 profile §(e) |
| Lock 16 SIMD/ASM admissibility allowlist (NEON 3-pack, dav1d primitive lineage) | Every `core::arch::*` use-site + every `asm!` block traces to a Lock 16 row with citation + checkasm parity admission | `restart/locks/14-LOCKS.md` §16 |
| Wave 1 → Wave 2 → Wave 3 → Wave 4 → Wave 5 ordering for arm64-host SOTA-BEAT close | Substrate + consumer same-wave per the `LESSONS-LEARNED.md:17-26` rule | `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §3-§7 |

The 5-shape taxonomy is kept *as a taxonomy*; `CollapsedStage` stays in the taxonomy as a derivable shape, even though its dispatch is deferred. This preserves the cost-model elegance and avoids re-architecting later.

---

## §6 — What to drop or amend from SK-V3

| Item | Disposition | Rationale |
|---|---|---|
| **`EventCursor` mask-driven dispatch (per Wave 2 Agent 4)** | DROP | Already refuted; the typed event cursor is over the retained tape projection (`OffsetTape`), not over a mask-driven event stream. SK-V3 § already incorporates this; verify no residual `EventCursor` references in Wave 1 implementation. |
| **`CollapsedStage` as part of SK-V3 close** | DROP from SK-V3 close; KEEP in taxonomy | Per §3 above; defer dispatch to successor tranche. Amendment §4.1. |
| **"Outclass asmjson by 1.28× on Zen 4 (14.0 GiB/s)" framing** | AMEND | Move from `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §8 (Wave 6 close condition) to a separate aspirational document. The 1.28× claim is conditional on cost-model derivation producing correctness-equivalent output to hand-written asmjson — a property no harness measures today. Without the harness, the claim is unverifiable. The SK-V3 close target is "BEAT sonic-rs / simdjson on M5 Max expanded corpus", not "BEAT asmjson on Zen 4". |
| **Wave 6 (x86_64 strict SOTA path) as a single wave** | SPLIT | Per §4.1 Amendment 2: split into Wave 6a (AVX-512 primitives consumed by `OffsetTape`, unconditional once x86_64 access exists) and Wave 6b (`CollapsedStage` backend, deferred to successor tranche with separate plan). |
| **Lock 16 AVX-512 5-pack** | KEEP, but re-frame | Land each primitive as a `bbnf-simd` standalone with scalar reference + checkasm parity. Consume from `OffsetTape` on x86_64. Decouple admission from `CollapsedStage` dispatch. |
| **"asmjson is a 9-state FSM" framing** | AMEND to "9-state DPDA with explicit stack" | Per FSM-audit §d. Documentation clarity; no architectural change. |

---

## §7 — What to redo differently

### 7.1 Wave 6 (x86_64 strict SOTA path) rewrite

Current §8 of `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` couples AVX-512 primitive admission to `CollapsedStage` dispatch. Rewrite to split:

**New Wave 6a (AVX-512 primitive admission, x86_64 strict)**:
- Owner paths: `skinny/crates/bbnf-simd/src/x86_64/`
- Adopt Lock 16's 5-pack (GFNI / k-mask / VPCLMULQDQ-512 / AVX-IFMA / VNNI / BITALG) as standalone primitives with scalar references in `scalar/` and checkasm parity in `tests/checkasm_parity.rs`.
- Consume primitives from the existing `OffsetTape` lowering pattern on x86_64 dispatch hubs. The existing structural-index-driven codegen template already has the access pattern; the primitives accelerate the underlying scan.
- Exit gate: `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity` returns 0 divergences; x86_64 row in `bench-json --native-sidecars` reports throughput improvement vs Wave 1+2 x86_64 baseline.
- **No `CollapsedStage` admission required for Wave 6a.**

**New Wave 6b (`CollapsedStage` backend) — deferred**:
- Successor tranche with its own plan document.
- Hard preconditions per §4.2.

### 7.2 The Lock 16 AVX-512 5-pack lands WITHOUT FSM context

Each primitive's admissibility row in `restart/locks/14-LOCKS.md` §16 stands on its own scalar reference + checkasm parity. Whether `CollapsedStage` consumes them in fused form is downstream — the primitives are useful for `OffsetTape` on x86_64 regardless of whether the FSM backend ever ships.

### 7.3 Add per-grammar `derive_backend_shape` firing matrix audit

As part of Phase 2 (LayoutFacts cost model + `Alt { Dispatch }` two-access-pattern lowering), emit a per-grammar audit table at codegen time showing:

| Grammar | Rule | first-set disjoint? | overlap? | recovery? | host fn? | layout? | target features? | → BackendShape |
|---|---|---|---|---|---|---|---|---|

This is the falsifiability gate against the per-grammar god-module pattern (Lock 14). If `CollapsedStage` fires for one grammar but not for any other, fold the verdict into the matrix and decide whether that's acceptable scope or a Lock 14 violation.

### 7.4 Add per-grammar consumer-same-wave verification

As part of every wave's exit gate, the wave must cite a runtime call site that consumes the substrate landed in the wave. Per `LESSONS-LEARNED.md:17-26` (the 2026-04-29 rule). Format: `samply leaf at <file:line> shows <bin>% self-time on <consumer fn>`. This formalises the Era V mitigation.

---

## §8 — Falsifiability gates added (distinguishing SK-V3 success from PSI-style failure)

New gates beyond the existing IMPLEMENTATION-PACKET wave exit gates. Each gate falsifies a specific PSI-recurrence shape.

| Gate | What it falsifies | Measurement |
|---|---|---|
| **G1: No OpenFrame in runtime** | F1 (parallel substrate) recurring at the cursor layer | `rg "OpenFrame\|Vec<OpenFrame>\|ParseStream" crates/runtime/src crates/codegen/src` returns zero. CI-gated. |
| **G2: BackendShape side-table is single-source** | F2 (type ambivalence) recurring via multiple decision surfaces | `rg "EmissionTier\|BackendShape\|emission_strategy" crates/ir/` returns exactly one decision surface (`LayoutFacts.backend_shape`). No companion field. |
| **G3: Wave consumer cite** | F3 (substrate-first/consumer-later) | Every wave exit cites a samply leaf at a consumer fn with ≥X% self-time. Wave exit blocks if no consumer cite. |
| **G4: No grammar names in generic crates** | F5 (per-grammar god-modules) | Lock 14 verification command `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>\|CssL4\s*=>\|Bbnf\w*\s*=>\|GoogleSheets\w*\s*=>' crates/{ir,bbnf-simd,bbnf-ser,passes}/` returns zero. CI-gated. |
| **G5: No runtime dispatch tables in hot path** | F6 (interpreter-dispatch ceiling) | `cargo asm` on the generated parser shows no `match table.states[N]` or equivalent runtime-LUT dispatch in the hot loop. Per-grammar audit. |
| **G6: BackendShape firing matrix shows ≥2 shapes across the 9-grammar matrix** | F2 (taxonomy redundancy at AQ.5 precedent) | `derive_backend_shape` audit emitted per grammar. If only one shape fires across all grammars, the taxonomy collapses per AQ.5. |
| **G7: Phase 4 (`CollapsedStage`) has its own differential parity harness BEFORE dispatch** | The codegen-emitted-FSM concern (skv3-diff-audit §d) | `tests/fsm_codegen_parity.rs` exists, runs cost-model-derived FSM bytecode vs hand-written reference, returns 0 divergences. Without this, Phase 4 is unfundable. |
| **G8: Phase 4 admission produces ≥1 non-JSON `CollapsedStage` firing OR records "JSON-only" as known scope** | Lock 14 god-module at cost-model level | `derive_backend_shape` audit table records per-grammar verdict; deviation routes to Lock 14 amendment. |
| **G9: Expanded SOTA gate on M5 Max** | Substrate viability for the SOTA-BEAT claim | `cargo run -p xtask --release -- check-conformance && bench-json && gate-json` produces 0 G-rows on the 17-corpus expanded gate; `random` and `unicode_escapes` no longer show `parse_value_at` as dominant samply leaf. |
| **G10: yyjson/simdjson/asmjson native sidecar comparators reported with strictness plane** | Honesty about comparator basis (per `feedback_no_warm_benches`) | RESULTS.md table has columns for strictness, mode, host CPU, and warm-vs-cold per-parse |

G1, G2, G4, G5 are CI-gated (block merge). G3, G6, G7, G8, G9, G10 are tranche exit gates.

---

## §9 — Pre-commit checklist

Before the SK-V3 wave dispatcher fires:

- [ ] **Has SK-V3 been audited against EACH of the 5 PSI failure modes?**
  - [x] 1.1 OpenFrame clone → VERIFIED-DIFFERENT (Lock 1 + grep gate G1)
  - [x] 1.2 Type ambivalence → SAME-CLASS-DIFFERENT-INSTANCE; mitigated by single side-table; gate G2 + G6 covers
  - [x] 1.3 Substrate-first/consumer-later → SAME-CLASS-DIFFERENT-INSTANCE; mitigated by per-wave consumer cite; gate G3 covers
  - [x] 1.4 Columnar SoA → VERIFIED-DIFFERENT (three-array structural projection ≠ per-kind SoA)
  - [x] 1.5 Per-grammar god-modules → VERIFIED-DIFFERENT (provisional); gate G4 covers
  - [x] 1.6 (NEW) Interpreter-dispatch ceiling → VERIFIED-DIFFERENT for `OffsetTape`; UNDETERMINED for `CollapsedStage`; gate G5 + G7 covers

- [ ] **Has the FSM/DTA correctness verdict been resolved?**
  - [x] FSM-audit §e: `CollapsedStage` is correct as taxonomy entry, correct in admission predicates, but must not block SK-V3 close.
  - [x] Phase 4 deferred to successor tranche with separate plan + hard preconditions (§4.1 Amendments 1-2).
  - [x] `OffsetTape` is the SK-V3 close path; sub-700 LOC closes the M5 Max expanded gate.

- [ ] **Has the codegen-emitting-FSM concern been audited?**
  - [x] FSM-audit §d + skv3-diff-audit §d both flag the concern.
  - [x] The "hand-written FSM for one grammar" vs "codegen-emit FSM derived from Grammar IR" leap is named and quarantined.
  - [x] Gate G7 (FSM codegen parity harness) is a hard precondition for any Phase 4 dispatch.
  - [x] Gate G8 (per-grammar firing matrix) prevents JSON-only `CollapsedStage` lockup.

- [ ] **Amendment list applied to `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`?**
  - [ ] Amendment 1 (Phase 4 NOT in SK-V3 close)
  - [ ] Amendment 2 (Wave 6 split into 6a unconditional + 6b deferred)
  - [ ] Amendment 3 (close-condition update at §0)
  - [ ] Amendment 4 (rename "FSM" → "DPDA" in `SOTA-BEAT-DESIGN.md` §5.1)

- [ ] **Falsifiability gates G1-G10 wired into CI and wave-exit gates?**

- [ ] **Recommendation echoed at top of `restart/HANDOFF.md`?**
  - [ ] TL;DR: GO on SK-V3 minus `CollapsedStage`. CONDITIONAL on `CollapsedStage` (separate tranche, separate plan).

If all checkboxes resolve to checked, dispatch SK-V3 Wave 0 through Wave 5 against the skinny workspace.

---

## Appendix — companion-report cross-reference

| Section | Primary source | Verification source |
|---|---|---|
| §1 (failure modes) | `/tmp/psi-failure-anatomy.md` §B + §A | `/tmp/psi-excavation-report.md` (e) + `/tmp/psi-archaeology-report.md` (b)(c)(d) |
| §2 (risk audit) | `/tmp/skv3-psi-diff-audit.md` (b)(c) | `/tmp/psi-failure-anatomy.md` §C + §D |
| §3 (FSM verdict) | `/tmp/fsm-correctness-audit.md` (a)(b)(c)(d)(e) | `/tmp/skv3-psi-diff-audit.md` (d) |
| §4 (amendments) | `/tmp/skv3-psi-diff-audit.md` (e) | `/tmp/fsm-correctness-audit.md` (e) |
| §5 (keep) | `restart/skinny/audit/SOTA-BEAT-DESIGN.md` §10 | `/tmp/psi-failure-anatomy.md` §C.4 |
| §6 (drop/amend) | `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §8 | `/tmp/fsm-correctness-audit.md` (e) + `/tmp/psi-failure-anatomy.md` §D.5 |
| §7 (redo) | `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §8 | this synthesis |
| §8 (gates) | this synthesis | derived from §1-§7 |
| §9 (checklist) | this synthesis | derived from §1-§8 |

Companion-report dependency residuals (from skv3-diff-audit §"Companion-report dependency residuals"):
- C4 verdict (was PSI FSM-based?) → resolved by FSM-audit §a: DTA = Direct-Threaded Automaton (threaded-code interpreter walking a tape projection); PSI = sidecar structural-index prepass. Both deleted at AX.W0b. NEITHER is a pure FSM; the DTA interpreter is the closer architectural ancestor to `CollapsedStage`, and the `dispatch_one` interpreter ceiling at 20-35% self-time is the load-bearing prior-evidence against codegen-emitted-FSM-class dispatch in the hot path.
- F2 lower-bound verdict (Era V type ambivalence) → resolved by `/tmp/psi-failure-anatomy.md` §B.2 verbatim: three representations (tape ↔ OpenFrame ↔ direct-to-struct).
- F3 sequencing verdict (was Era V actually substrate-first-consumer-later in the same sense SK-V3's Wave 3 is at risk of being?) → resolved by `/tmp/psi-excavation-report.md` (e) verbatim Lock 1: substrate-first/consumer-later named as one of four canonical failure modes. Gate G3 (per-wave consumer cite) prevents recurrence.
