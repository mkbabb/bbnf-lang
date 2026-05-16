# SK-V3 vs PSI-failure-modes — architectural diff audit

Date: 2026-05-12
Cap: 40 min
Companion reports: `/tmp/psi-excavation-report.md`, `/tmp/psi-archaeology-report.md`, `/tmp/psi-failure-anatomy.md` did NOT land within the cap. This audit proceeds on the in-tree authority docs + the in-tree archaeology at `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md`. Where a verdict would have required companion-report evidence I do not have, it is marked **UNDETERMINED**.

## (a) SK-V3 load-bearing claims enumerated

The following are the load-bearing architectural claims SK-V3 advances. Path:line for each.

| # | Claim | Where stated |
|---|---|---|
| C1 | **Tape ≡ structural projection; one substrate, no parallel sidecar.** "There is no parallel sidecar structural-index `Vec`; if structural offsets are retained, they ARE the tape." | `restart/skinny/SUBSTRATE.md:217`; `restart/locks/LOCKS.md:34` (Lock 1 verbatim); `restart/skinny/tranches/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md:60-65` |
| C2 | **Five-shape `BackendShape` taxonomy with cost-model-derived per-rule selection.** `EagerTape | OffsetTape | EventTape | SinkOnly | CollapsedStage`; 8-priority derivation algorithm; no directive. | `restart/ARCHITECTURE.md:1047-1083`; `restart/skinny/SUBSTRATE.md:213-219`; `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:208-238` |
| C3 | **No new BIR variant; no new BBNF directive.** "The 20-variant BIR alphabet defined in `restart/ARCHITECTURE.md` §7.2 is preserved verbatim." Existing `Alt { Dispatch }`, `SimdScan`, `TapeEmit`, `DirectBuild` lower differently based on `LayoutFacts.backend_shape`. | `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:32-37`; `restart/skinny/tranches/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md:190-194`; `restart/skinny/tranches/WAVE-1-2-COHORT-DIGEST.md:75-82` |
| C4 | **9-state FSM + PC-as-state via `r10` indirect jump for `CollapsedStage`.** Adopt asmjson architecture verbatim; selected by cost model when `target_features.has("avx512vbmi2") AND rule is hub with ≥4 byte-disjoint arms`. | `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:478-489`; `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:265-283`; `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:229-232` |
| C5 | **Two NEON kernels close the residual G/NoGo corpora.** Class A `match_tiny_plain_string` 16-byte class check; Class B `vqtbl1q_u8`-driven `\uXXXX` hex decode. Both gated by `bbnf-simd` checkasm differential parity harness. | `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:194-227`; `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:117-142` |
| C6 | **`bbnf-simd` is the grammar-neutral primitive crate**, organised by host (`aarch64/`, `x86_64/`, `scalar/`), with `xtask primitive-checkasm` admission gate against a scalar reference. No grammar names in this crate. | `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:339-376`; `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:62-94`; `restart/locks/LOCKS.md:60` (Lock 14) |
| C7 | **AVX-512 esoterica stack additive ATOP asmjson architecture**, not in lieu. GFNI / k-mask family / VPCLMULQDQ-512 / AVX-IFMA / VNNI / BITALG; each "replaces a specific asmjson primitive with a strictly fewer-µop equivalent." | `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:491-505`; `restart/locks/LOCKS.md:80-86` (Lock 16) |
| C8 | **Substrate-first/consumer-later anti-pattern forbidden by Lock 1.** "Plans that implement tape with consumer-later sequencing are faults; plans that implement tape properly with same-wave consumer wiring + direct-to-struct union are honoured." | `restart/locks/LOCKS.md:34` (Lock 1 verbatim) |
| C9 | **Expanded SOTA gate is binding**, not the historical triad. `random / unicode_escapes / y_string_unicode / github_events / update_center` define G/NoGo. | `restart/skinny/tranches/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md:13-17`; `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:7-29` |

## (b) PSI-failure-modes → SK-V3 correspondence

Failure-mode taxonomy is reconstructed from `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md` (the in-tree archaeology) absent the requested `/tmp/psi-failure-anatomy.md` companion report. The five named failure classes from the user's prompt map as follows.

| # | PSI failure mode | Anchor citation | SK-V3 surface affected | Direct verdict |
|---|---|---|---|---|
| F1 | **OpenFrame clone parallel substrate** (the 86.07% samply pathology) | `docs/tranches/PLAN-INPUT-2026-05-03.md:52`; `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:90-100` (AW-I DTA walker carrying structural overhead the `fn __<rule>` path did not) | C1 (one substrate); C2 (`OffsetTape` taxonomy entry) | C1 is explicit verbatim refutation: `restart/skinny/SUBSTRATE.md:217` says *"no parallel offset stream"*. C2's `OffsetTape` is the ONE substrate, not a side-by-side `Vec<OpenFrame>` alongside the source. **F1 is addressed.** |
| F2 | **Type ambivalence** (tape ↔ OpenFrame ↔ direct-to-struct competing for the same role) | `era-V-dta-psi-rut.md:103-115` (AW-II: "viability question raised for the successor tranche"); `era-V-dta-psi-rut.md:128-145` (AW-III: substrate landed on 5 emitter-mined consumers, none reached the gate) | C2 5-shape taxonomy: `EagerTape / OffsetTape / EventTape / SinkOnly / CollapsedStage` over the SAME `Tape<'input>` | **NOT VERIFIED.** `restart/ARCHITECTURE.md:1047-1083` defines five distinct backend shapes selected per-rule. SK-V3 frames this as "five projections of one substrate" but the lowering matrix at `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:212-236` (§4.1) emits *materially different generated code* per shape — `EagerTape` reads `source[pos]`, `OffsetTape` reads `source[offsets[*cursor as usize] as usize]`, `CollapsedStage` is an FSM with `r10` indirect jump. **These three are not "the same parser with different ValueRef cursors"; they are three different parsers selected at codegen time.** See (c) verdict for C2. |
| F3 | **Substrate-first/consumer-later (Era V)** | `era-V-dta-psi-rut.md:78-86` (AV: "The substrate landed; the activation sits one cherry-pick behind"); `:188` (AW-V: "demonstrated thesis and lost it within its own tranche") | C8 (Lock 1 codifies the prohibition); IMPLEMENTATION-PACKET wave sequencing | **MIXED.** Lock 1 verbatim states "substrate-first/consumer-later (Era V failure mode)" is a fault (`restart/locks/LOCKS.md:34`). But IMPLEMENTATION-PACKET §6 (Wave 4) defines `parse-that/{string,unicode,number}` as a separate wave from Wave 3 (`bbnf-simd` primitive contract) and Wave 1 (typed event cursor) — those are three substrate slabs ordered before Wave 5 (workload gates), which is the first place anything claims to test direct-to-struct as a consumer of all of it. The cost-model `derive_backend_shape` (Lock 10) is the implicit consumer-binding, but the wave dispatcher does not exhibit "ship substrate + its consumer in the same wave" verbatim. See (c) verdict for C8. |
| F4 | **Columnar SoA never activated** | `era-V-dta-psi-rut.md:64-67` (Y tranche "Tape column splits — survive into AU then revert in AY-I.W1"); `era-IV-tape-first.md:36-40` | C1 explicitly forbids columnar SoA revival | The structural index (offsets + flags + payload arena) is *structurally* AoS-of-tokens, not column-per-field SoA. `restart/skinny/SUBSTRATE.md:217` says *"`Tape<'input>` owns the offsets array, the payload arena, and the per-offset packed flags as one structure"* — three arrays carried together, not SoA-per-grammar-rule. **F4 is addressed.** The three-array shape (offsets + flags + payload arena) is what simdjson, asmjson, yyjson, and sonic-rs all retain; it is not the dead AV.04 design. |
| F5 | **Per-grammar god-modules in generic crates** | `era-V-dta-psi-rut.md:55-69` (V1's `GrammarProfile` const with 17 fields, 7 of which became dead) | C6 (`bbnf-simd` grammar-neutral); Lock 14 verbatim | C6's design says zero grammar names in `bbnf-simd`. The Class A kernel (§3.2.1) is described as *"the JSON-string-body alphabet LUT"* — JSON-named in description (`restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:121`), but the implementation crate `bbnf-simd/aarch64/string_block.rs` is grammar-neutral by trait surface (`SimdClassifier::classify_chunk`). **F5 is addressed at the crate boundary**, but the cost-model heuristic that picks `OffsetTape` for "byte-finite disjoint first-set" is going to land identical-looking code for JSON, CSS, BBNF, Sheets — which is the GOAL (Lock 14), not a defect. |

## (c) Per-claim verdict

Per-claim verdict against the failure-mode taxonomy:

| Claim | Verdict | Reasoning |
|---|---|---|
| C1 (one substrate) | **VERIFIED DIFFERENT** | `restart/skinny/SUBSTRATE.md:213-228` directly cites the pre-lazy-implementation's three parallel offset buffers (`ParserState.structural_offsets`, `TapeAssembler.offsets`, `Tape.offsets`) as "implementation drift, not a Lock 1 defect" and prescribes a concrete migration: scan emits *through* `TapeBuilder` which is "a thin facade over `Tape::offsets` during construction." This is mechanically distinct from F1's `Vec<OpenFrame>::clone` pathology because there is no second collection to clone from. The structural projection IS the tape. |
| C2 (5-shape taxonomy) | **SAME-CLASS-DIFFERENT-INSTANCE** | Architecturally the 5-shape `BackendShape` taxonomy *resembles* the AV/AW EmissionTier 3-shape taxonomy (`MustTape / MustFn / MayInline`) that Era IV's AQ.5 (`2f7c1bd4`) **deleted as orthogonal-decision-surface** per `era-IV-tape-first.md:62-70`. The PSI/DTA arc shipped multiple orthogonal "tier" surfaces; AQ.5's "never two decision surfaces for one semantic" lesson is enshrined as the `no-orthogonal-codepaths` feedback memory. SK-V3 mitigates this risk by housing `backend_shape` in `LayoutFacts` as a SIDE TABLE (`ARCHITECTURE.md:1034`, "Public"), and pivoting on a SINGLE 8-priority derivation algorithm (`ARCHITECTURE.md:1074-1082`). This is structurally different from EmissionTier — fewer surfaces (one decision = `backend_shape`, not two = `EmissionTier × StructuralDispatch`) — but the same architectural shape (an enum that branches codegen). Falsifiable risk: if any of the 5 shapes proves redundant (e.g., `EventTape` collapses to `OffsetTape` once the typed event cursor lands), this is the AQ.5 lesson recurring. **The risk is not eliminated; it is gated on the derivation algorithm staying single-source.** |
| C3 (no new BIR variant) | **VERIFIED DIFFERENT** | The current BIR alphabet is preserved (20 variants per ARCH §7.2). The lowering is *pattern* (`Alt { Dispatch }` → three access patterns), not *variant* (`Alt { Dispatch }` vs `Alt { Dispatch, BackendShape: CollapsedStage }`). This is concretely distinct from Era V's pattern of growing `EmissionTier` as a new IR-level axis. The verification gate is `rg -n "@(simd|runtime|backend|shape|asm)" grammars restart/skinny` returning zero (`IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:34`). |
| C4 (9-state FSM + PC-as-state) | **UNDETERMINED** | This is the load-bearing claim that companion-report archaeology was supposed to adjudicate. The in-tree archaeology at `era-V-dta-psi-rut.md` does NOT report whether PSI was FSM-based or not — it describes PSI as "Parallel Structural Index" (line 33), which is a parallel-parse pre-computed index, not a state machine. **If PSI was index-based and not FSM-based, then C4 is a NEW lever that has no prior counter-evidence; if PSI shipped an FSM in any wave, C4 is recapitulation.** See (d) for the codegen-emits-FSM concern this raises independently. |
| C5 (two NEON kernels) | **VERIFIED DIFFERENT** | The Class A and Class B NEON kernels are not architectural; they are pathology-class-specific scalar-loop replacements gated by `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity` (`IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:232-238`). The dav1d/FFmpeg discipline transfer (Wave 1 Agent 2) wraps every primitive in a scalar-reference parity test before admission. Era V had no such admission gate; `escape_mask_64`'s correctness bug surfacing on first run of the harness (Wave 2 Agent 5) demonstrates teeth. |
| C6 (bbnf-simd grammar-neutral) | **VERIFIED DIFFERENT** (provisional) | The crate shape at `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:62-94` carries no grammar names; the trait surface is `SimdClassifier { fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult }`. Lock 14 verification command `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>\|CssL4\s*=>\|Bbnf\w*\s*=>\|GoogleSheets\w*\s*=>' crates/` must return zero. **Provisional because the crate is not yet built; it could regress.** The skinny-local equivalent at `skinny/crates/bbnf-simd/` already exists per the git status and is gated by the same checkasm harness. |
| C7 (esoterica strictly atop asmjson) | **VERIFIED DIFFERENT** | Each esoteric primitive is described as a REPLACEMENT for a specific asmjson primitive (table at `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:494-501`): `GFNI vgf2p8affineqb` replaces `6× vpcmpeqb`, `k-mask family` replaces `4 store+load round-trips/chunk`, etc. This is additive (each is independently falsifiable via `bbnf-checkasm`) rather than alternative. The asmjson architecture itself (9-state FSM) is preserved as the floor. |
| C8 (no substrate-first/consumer-later) | **NOT FULLY VERIFIED** | Lock 1 codifies the prohibition at `restart/locks/LOCKS.md:34`. But the IMPLEMENTATION-PACKET §3-7 wave structure does not exhibit "substrate + consumer same wave" verbatim. Wave 1 (typed event cursor) ships the EventCursor adapter, fixes the Class A/B NEON kernels, AND re-baselines `parse_value_at` — that is substrate + consumer same wave. Wave 2 (capacity policy) is the rejected-route ledger landing tranche. Wave 3 (`bbnf-simd` kernel contract host-aarch64 first) is substrate-without-direct-consumer; it relies on Wave 1 having already wired the consumers. Wave 4 (`parse-that` primitives) ships substrate + their consumers (view accessors). Wave 5 (workload gates) is purely consumer-side. **Substrate-first-consumer-later is not eliminated; the risk concentrates on Wave 3's `KernelSet` + `PrimitiveKernel` trait surface** — if Wave 3's deliverables stagnate without same-tranche consumers, that is the Era V pattern recurring. The mitigation visible: Wave 1 explicitly ties its NEON kernels to specific generated.rs:161-172 call sites with samply leaf attribution. **Mitigated, not eliminated.** |
| C9 (expanded gate is binding) | **VERIFIED DIFFERENT** | `restart/skinny/tranches/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md:13-17` carries split verdict (triad pass, expanded G/NoGo); the historical triad does NOT carry the close. This is structurally distinct from Era IV's reliance on `post-AU.json` as the single comparison anchor (`era-IV-tape-first.md:101-127`). The pathology-class diagnosis (Wave 2 Agent 2 partitioning the 5 failing corpora into Class A `tiny_string` + Class B `hex_decode`) is the granular sub-anchor that prevents single-corpus overfit. |

## (d) The codegen-emits-FSM concern (load-bearing risk not previously audited)

**This is the audit's most important novel finding.**

C4 commits SK-V3 to **codegen-emitting an FSM that the cost model selects on a per-rule basis**. The supporting evidence in the authority docs is:

- `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md:478-489`: "9-state FSM (object-key / object-colon / object-value / array-value / string / number / literal / pre-comma / pre-close); PC-as-state dispatch via `r10` register holding the next state's code address, `jmp r10` per transition"
- `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:281-283`: "Each state has its own classifier mask set... State transitions happen by jumping to the state's entry label after each chunk's classification, with `r10` holding the next-state target across chunk-refetch boundaries. No state-variable memory traffic; the program counter *is* the state."
- `restart/ARCHITECTURE.md:1080-1082`: derivation step 6: "Else if target features admit AND rule is a hub with ≥ 4 byte-disjoint arms ⇒ `CollapsedStage`"

The asmjson reference is **a hand-written .S file** for one grammar (JSON). The PC-as-state direct threading is hand-written assembly — the assembler chooses the addresses, the human-author wrote the state transitions. SK-V3 proposes to **generate this from grammar shape**.

The qualitative leap not audited:

1. **Hand-written FSM vs codegen-emitted FSM are different problems.** The FSM that asmjson uses is JSON-specific: 9 states correspond to JSON's specific grammar shape (object-key / object-colon / object-value / array-value / string / number / literal / pre-comma / pre-close). For grammars with other shapes — CSS L4 with @-rules, BBNF-self with Pratt operators, Sheets with array literals — the state alphabet is *different*. The codegen template that emits the FSM must derive the state alphabet from Grammar IR.
2. **PC-as-state requires control over generated code addresses.** `jmp r10` is direct-threaded interpretation that depends on the assembler placing labels at known addresses. Rust codegen via LLVM does not give the bbnf-codegen layer that control. The fallback is `match next_state { K => goto K, ... }` which LLVM may or may not compile to indirect branches; the M5 Max NEON path (which is the primary host) does not have `jmp r10`-equivalent at the intrinsics layer.
3. **The 8-priority `derive_backend_shape` algorithm has no row that handles "the FSM the cost model wants to emit cannot be realised on this host."** The fallback path is implicit (falls through to `OffsetTape`), but the FSM derivation logic itself must be implementable from Grammar IR, which is not demonstrated anywhere in the authority docs.

**This is the failure-mode signature of Era V's AW-V**, per `era-V-dta-psi-rut.md:187-188`: "AW-V's thesis — 'auto-derive the sonic-rs-class inner loop from any BBNF grammar' — was demonstrated exactly once, on JSON, at W3 close (commit `c1e86ab3`), and lost by W6." The structural parallel:

- AW-V: auto-derive sonic-rs-class inner loop from BBNF → JSON only, lost within tranche
- SK-V3: auto-derive asmjson-class 9-state FSM from BBNF → JSON only on Phase 4, gated on Zen 4 AVX-512 silicon access (`WAVE-1-2-COHORT-DIGEST.md:232-241`)

**This is the same risk shape as AW-V's auto-derive thesis.** The mitigations SK-V3 carries that AW-V did not:

- Phase 4 is conditional: gated on x86_64 AVX-512 silicon access (`WAVE-1-2-COHORT-DIGEST.md:232-241`) — currently UNAVAILABLE.
- Phase 4 LOC budget ~600 (`SOTA-BEAT-DESIGN.md:352`) — modest.
- Phase 1/2/3 do NOT require the FSM (the SOTA-BEAT close for sonic-rs/simdjson on M5 Max is Phase 1 + Phase 2 alone; ~700 LOC NEON + codegen template).

**Verdict on (d):** The codegen-emitting-FSM concern is REAL but is QUARANTINED to Phase 4 which is conditional. SK-V3 close for sonic-rs/simdjson on M5 Max does NOT require the FSM derivation; it requires the typed-event-cursor lowering pattern (C2's `OffsetTape` access) + the two NEON kernels (C5) + Lock 15 build-profile discipline. **As long as Phase 4 stays gated on silicon access AND the close criterion remains "expanded SOTA gate on M5 Max", the FSM derivation is not on the critical path.** The risk is that Phase 4 gets dispatched anyway and recapitulates AW-V.

## (e) Honest verdict

**Proceed with SK-V3 as planned, with one concrete edit to the IMPLEMENTATION-PACKET.**

The audit verdicts above net out as:

- **5 VERIFIED DIFFERENT**: C1, C3, C5, C7, C9
- **1 VERIFIED DIFFERENT (provisional)**: C6
- **2 SAME-CLASS-DIFFERENT-INSTANCE / NOT-FULLY-VERIFIED**: C2, C8
- **1 UNDETERMINED**: C4 (FSM is/is-not PSI-recapitulation)

The PSI failure modes per the era-V archaeology + `feedback_no_orthogonal_codepaths` are mitigated structurally by:

1. **Lock 1's verbatim "no parallel offset stream"** addresses F1 (OpenFrame clone parallel substrate).
2. **The 5-shape `BackendShape` as a `LayoutFacts` side-table with single-source-of-truth derivation** is a structurally different architecture from EmissionTier's two-axis (`MustTape × StructuralDispatch`) decision surface that AQ.5 deleted.
3. **The checkasm differential parity harness** (Wave 2 Agent 5; `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`) is the dav1d/FFmpeg discipline transfer that Era V lacked; it caught `escape_mask_64`'s correctness bug on first run.
4. **The pathology-class diagnosis** (Wave 2 Agent 2: Class A `tiny_string_loop` + Class B `hex_decode`) is granular sub-anchor evidence that the failing corpora are localised implementation gaps, not substrate faults.
5. **The win column** (4/17 corpora at M5 Max anchor per `skinny/profile/native-sidecars/PROFILE-REPORT.md`) forbids any redesign of Lock 1 (lazy-offset tape).

The risks that remain:

- **Risk R1**: Phase 4 (`CollapsedStage` AVX-512 backend) is the AW-V auto-derive recurrence vector. **Mitigation**: keep Phase 4 gated on Zen 4 silicon access; do NOT dispatch it as part of SK-V3 close.
- **Risk R2**: Wave 3 (`bbnf-simd` kernel contract host-aarch64-first) is a substrate-without-direct-consumer slab if Wave 1 doesn't already wire it. **Mitigation**: Wave 1's Class A + Class B NEON kernel landing sites are explicit (`generated.rs:161-172` + `parser.rs:78-113`); Wave 3 lands the dispatch infrastructure but the consumers are Wave 1's kernels. The wave order is Wave 1 → Wave 3 → Wave 4 in IMPLEMENTATION-PACKET; this is correct.
- **Risk R3**: The 5-shape taxonomy resembles EmissionTier. **Mitigation**: `LayoutFacts.backend_shape` is a single side-table field, derivation is one 8-priority algorithm, no companion field. If any of the 5 shapes proves redundant during implementation, collapse on AQ.5 precedent.

**Recommended edit to IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md**:

Add an explicit non-negotiable at §1 (after line 37):

> **Phase 4 (`CollapsedStage` AVX-512 backend) is conditionally dispatched only after Phase 3 lands on Zen 4 silicon.** No SK-V3 close gate requires Phase 4. If Zen 4 silicon access does not materialise within the SK-V3 cap, Phase 4 is deferred to a successor tranche with its own plan document, not folded into a successor wave of SK-V3. This guards against the AW-V auto-derive failure-mode (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:187-188`).

This is the surgical edit that prevents the most-likely failure-mode recurrence. The remainder of SK-V3 — typed event cursor over the retained tape projection + two NEON kernels checkasm-gated + `LayoutFacts.backend_shape` cost model + Lock 15 build-profile discipline + Lock 16 admissibility allowlist — is the empirical synthesis of the Wave 1/Wave 2 cohort and is structurally distinct from the PSI/DTA failure-mode anatomy.

**Do not course-correct. Dispatch SK-V3 with the §1 amendment.**

## Companion-report dependency residuals

The following audit items would have been definitively resolved by the companion reports that did not arrive:

- C4 verdict (was PSI FSM-based or index-based?) → currently UNDETERMINED; defaults to (d)'s codegen-emits-FSM analysis on its own merits.
- F2 lower-bound verdict (did Era V actually have type ambivalence in the OpenFrame ↔ tape ↔ direct-to-struct sense?) → the archaeology gives indirect evidence (AW-V's "shape-emitter substrate landed for all grammars but only JSON's `parse()` routes through it at runtime"; `era-V-dta-psi-rut.md:178-185`); the 5-shape taxonomy's defensibility is more contingent than this audit can fully establish without the companion failure-mode anatomy.
- F3 sequencing verdict (were Era V's substrate waves actually substrate-first-consumer-later in the same sense SK-V3's Wave 3 is at risk of being?) → the archaeology says yes for AV/AW-I/II/III/IV/V; whether SK-V3's Wave 3 is the same shape requires comparison against the specific commits in those tranches the companion reports were to excavate.

If the companion reports surface post-cap, re-run the (c) and (d) sections with their evidence; the (e) verdict's robustness to that evidence is conditional on Phase 4 staying quarantined.
