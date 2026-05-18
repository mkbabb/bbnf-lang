# SK-V9 S-P3 CHALLENGE V2 — CH5 HIDDEN COUPLING

Lens: CH5 — HIDDEN COUPLING (`ORCHESTRATOR` §3W). Cycle: V2.
Date: 2026-05-18.
Cohort under audit: SK-V9 S-P3 — the seven P3 artefacts at
`restart/skinny/tranches/sk-v9/research/p3/` after the V2 integration
fold (commit `ef40c0fc`, `docs(sk-v9-p3-v2): integrate P3-A..E into the
SPEC + DISPATCH drafts`).
Disposition: ONE file; V1-resolution table + V2 disposition table; no
commit.

## §0 — Method and V1 carry

V1 returned CH5 at 44% (12 ACCEPT / 9 REVISE / 6 REJECT). The verdict
was bimodal: the P2-A union event-model itself — W3 in isolation — was
coupling-clean, but the wave plan that landed it was not. The six
REJECT rows (D6, D7, D8, D11, D24, D25) and three of the nine REVISE
rows (D4, D20, D26) all traced to **one root cause**: P3-B/C/F split
the P2-D cascade-locked block across waves W3/W4/W5 and propagated the
split into the SPEC entry-gate language and the dispatch contract,
shipping a REDRESS-82 parser-owned-scratch shape at W5. The V1 V2-fold
list named seven items (V2-CH5-1 through V2-CH5-7).

CH5 V2 verifies the cohort against the seven V2-fold items and the six
task-specified verification lenses. The substrate-cardinality invariant
remains Lock 1 verbatim: "A SIMD mask stream is a transient producer,
not a retained sidecar; if structural offsets are retained, the
structural projection IS the tape." The P2-D §0 V2-fold footer
constraint — "P2-A must land in the same wave as any of these P2-D
consumer slices … the wave may not be split" — is the disambiguation
target.

Verdict legend unchanged: ACCEPT — coupling-clean. REVISE — sound,
defective in a citable particular. REJECT — re-opens Lock 1 or a
REDRESS sidecar route.

## §1 — V1 REJECT + REVISE resolution (the seven V2-fold items)

| Item | V1 fault | V2 resolution | Status |
|---|---|---|---|
| **V2-CH5-1** (REJECT, blocking — re-merge / cascade-honour) | P3-B §2 split C3→W3, C5→W4, C4→W5, C6/C7 unplaced, with no cascade-constraint mention; D6 called it the central CH5 fault. | The V2 fold did NOT re-merge into one monolithic wave — it took the consolidation's item-6 + item-7 route: W3 lands the union; W4 is **sub-waved W4a-d**; each W4 sub-wave wires its kernel into the *already-landed* W3 union same-commit. `skv9-p3-F-spec-draft` §2.2 ("The Cascade-Lock, Disambiguated") states the binding reading verbatim: the constraint "means a P2-D kernel must not land **without the union substrate existing**. It does NOT mean one monolithic redress wave." The manifest (§2 table) places C3→W3, C5→W4a, C4→W4b, C6→W4c, C7→W4d — every cascade-locked candidate is now placed; none is unplaced. The split-vs-no-split conflation is closed. | **RESOLVED** |
| **V2-CH5-2** (REJECT, blocking — strike post-W3 entry-gate language) | `skv9-p3-F-spec-draft` §7 "W4 cannot dispatch before W3 closes / the W3 union is the consumer base"; dispatch-draft "W4 Conditional on W3 close, W5 Conditional on W4 close" reified the split into a non-negotiable-adjacent sentence. | The V2 SPEC §7 preamble now reads "each sub-wave's redress wires its kernel into the **already-landed W3 union substrate** in the same commit (the cascade-lock, §2.2)". The dispatch-draft Wave Manifest reads "W4a … Conditional on W3 close (cascade-lock)" with the §2.2 cross-reference and the explicit "It does NOT mean one monolithic wave" gloss. The "consumer base" phrasing survives once (§2 table W4a cell: "union substrate is the consumer base") but it is now *correct* — the union substrate IS the same-commit consumer for each W4x kernel, and §2.2 disambiguates the relation. Entry-gate language no longer encodes a cascade *violation*. | **RESOLVED** |
| **V2-CH5-3** (REJECT, blocking — restore the union-substrate codec consumer slice) | `skv9-p3-A` C4 and `skv9-p3-F` §8 dropped the P2-D §3.5 "union-substrate codec consumer (per-tape-cell projection), 80-150 LOC, co-developed with P2-A" slice; D11/D20 called it the Lock-1 re-opening. | The V2 fold takes the **explicit-second-branch** route the V1 fold offered ("if the cohort means the codec to consume *only* the parser-owned x4 path — state that explicitly and carry the REDRESS-82 differential proof"). P3-A C4 now states plainly: "The **production consumer** is the JSON materialiser `unescape_string` — specifically the already-wired `unescape_four_unicode_escapes` x4 path at `parse-that-regex/src/lib.rs:402` … The wave **re-bodies an existing production path; it does not introduce a new consumer**." The two-slice P2-D §3.5 ambiguity is collapsed to ONE consumer (the already-wired x4 path), and `runtime/src/grammars/json/sink.rs` is added to the W4b owner table as the call-site swap. The REDRESS-82 five-axis differential is carried verbatim in `skv9-p3-F-spec-draft` §7.2 pre-blocked-routes and `skv9-p3-E` §3.4. **The slice is not dropped — it is collapsed and the disposition stated.** See D12 for the residual coupling check this raises. | **RESOLVED (with carried check D12)** |
| **V2-CH5-4** (REVISE — place or defer C6/C7) | `skv9-p3-B` manifest placed neither C6 (EOR3) nor C7 (CTZ); P3-E carried no deferral entry; D9/D10/D17 flagged the silent drop. | C6 is placed in W4c (`skv9-p3-F-spec-draft` §7.3 "SHA3 EOR3 Prefix-XOR Ladder") and C7 in W4d (§7.4 "CSSC CTZ String-Mask Consumer"). Both carry full owner tables, exit gates, entry gates, and pre-blocked-route lists. No deferral entry is needed — neither candidate vanished. The dispatch-draft Wave Manifest lists W4c and W4d as discrete triumvirates. | **RESOLVED** |
| **V2-CH5-5** (REVISE — name the three "same-wave" relations distinctly) | "same-wave" overloaded across cascade-lock / per-kernel consumer / paired-admission; D26 said the overload let the cohort drop the cascade. | `skv9-p3-F-spec-draft` §2.2 closes with an explicit three-relation enumeration: (1) **Cascade-lock** — a P2-D kernel lands only after the W3 union substrate exists; (2) **Same-wave consumer** — every primitive and its hot-path caller in one commit; (3) **Codec/scanner pairing** — W4a+W4b strictly adjacent because neither closes the four uncloseable rows alone. The dispatch-draft repeats the cascade-lock vs strict-pairing distinction. The vocabulary is now disambiguated. | **RESOLVED** |
| **V2-CH5-6** (REVISE — scope `union_class_column_status` to every class-column consumer wave) | V1 D13: `skv9-p3-F` V1 §0.4 added a `union_class_column_status` field scoped W3-only; a cascade-locked downstream wave would need it gate-consumed at the consumer wave too. | The V2 fold pins the schema at the **36-identifier set** (`skv9-p3-D` §2.2; `skv9-p3-F-spec-draft` §0.y) and explicitly forbids a 37th column. `union_class_column_status` was a P3-F-V1 invention; it is **not** in the 36-field set and has been removed. The class-column substrate identity is instead carried by the existing `structural_projection_status` + `substrate_cardinality` + `substrate_surface` fields, which §0.y lists and §6 exit-gate clause 8 binds at W3. V1 D13 itself noted "on the merged-wave fold this is moot (one wave)" — the sub-wave fold makes it moot a different way: the substrate fields are populated at W3 (where the column is *created*) and the W4x sub-waves consume the *substrate*, not a new telemetry column. No field-scoping defect remains. | **RESOLVED (field removed, not re-scoped)** |
| **V2-CH5-7** (REVISE — re-check the no-Omega claim against the merged wave) | V1 D3 caveat: P3-B §4's "W3 needs no SC-6-L1-R1" held for W3-in-isolation; on a merged wave the union becomes the per-tape-cell codec consumer's production substrate and the claim needed re-statement. | The V2 fold did not produce a merged wave — W4b's codec consumer is now explicitly the **already-wired parser-owned x4 path** (V2-CH5-3 resolution), NOT a per-tape-cell union-substrate projection. The "per-tape-cell projection" slice that would have made the union a *new* production substrate for the codec is gone. Consequently the union substrate's role under the sub-wave fold is unchanged from W3-in-isolation: it is the offset-tape-with-class-column, consumed by the W3-internal `at_cursor` rebody. `skv9-p3-B` §4's no-Omega verdict therefore still holds as written — and the V2 SPEC §1 / §6 carry the "no `UnionTape`, no new `BackendShape`, Lock 1 cardinality stays at one" language without an SC-6-L1-R1 dependency. The V1 caveat is dissolved by the architecture, not patched. | **RESOLVED (caveat dissolved)** |

All seven V2-fold items resolve. None is left open. The resolution
route diverged from the V1 fold's literal recommendation in two places
— V2-CH5-1 (sub-wave fold, not monolithic re-merge) and V2-CH5-3
(collapse-to-one-consumer, not restore-the-second-slice) — but both
divergences are the *consolidation's* item-6/item-7 route, are
coupling-honest, and are verified clean in §2 below.

## §2 — V2 disposition table

| # | Subject | Finding | Verdict |
|---|---|---|---|
| V1 | Cascade-lock disambiguation — V2 SPEC must state P2-D §0 "may not be split" means a kernel may not land WITHOUT the union substrate, satisfied by W3 ≺ W4, NOT one monolithic wave | `skv9-p3-F-spec-draft` §2.2 quotes P2-D §0 verbatim then states the binding reading in a blockquote: "The constraint means a P2-D kernel must not land **without the union substrate existing**. It does NOT mean one monolithic redress wave." It is "satisfied by **W3 (the union event-model) preceding W4a-d**." Exactly the task-required disambiguation, in the SPEC, load-bearing. | ACCEPT |
| V2 | Each W4 sub-wave wires its kernel into the already-landed W3 union same-commit — no orphan | §2.2: "Each W4 sub-wave's redress commit then wires its kernel into that already-landed W3 union **in the same commit**: the consumer exists, the caller is wired same-commit, no orphan ships." §7 preamble repeats it per-sub-wave. The dispatch-draft Phase-3 protocol: "For a W4 sub-wave it wires the kernel into the already-landed W3 union substrate in the same commit." The same-commit consumer wiring is bound at three sites. | ACCEPT |
| V3 | W4a (32-byte string-block) — kernel + consumer same-commit | `skv9-p3-F-spec-draft` §7.1 owner table: `scan_string_special_block_32` in `bbnf-simd/src/aarch64/string_block.rs`; the consumer is `match_string_at_quote_trusted_utf8` (`parse-that-regex/src/lib.rs:162`) "producer-site rewire … the same-wave consumer". Exit gate clause 3: "`match_string_at_quote_trusted_utf8` is rewired to the 32-byte block in the same commit; the consumer call shows in the `samply` symbol path." This is the C5 candidate whose primary consumer is a pre-existing live call site (D21 V1) — genuinely non-orphan, replacement-not-addition. | ACCEPT |
| V4 | W4b (codec) — kernel + consumer same-commit; no parser-owned-scratch REDRESS-82 shape | §7.2 owner table: kernel in `bbnf-simd/src/aarch64/escape_codec/`; consumer is the already-wired x4 path at `parse-that-regex/src/lib.rs:402` re-bodied onto the kernel + the `Some(b'u')` arm at `:718-810` + the `runtime/src/grammars/json/sink.rs` call-site swap. The V2 fold collapsed the two-slice ambiguity (D11 V1) to one consumer — the pre-existing production path. "The wave re-bodies an existing production path; it does not introduce a new consumer" (P3-A C4). REDRESS-82's parser-owned per-quartet classifier is **not** re-introduced: the differential (§7.2 pre-blocked routes) states the 4-quartet batched path is the production path and the single-quartet binding fires only on pre-filter reject. No parser-owned-scratch shape. | ACCEPT |
| V5 | W4c (EOR3) — kernel + consumer same-commit | §7.3: `veor3q_u8` ladder replaces the scalar shift-XOR ladder *inside* `bitmap_prefix_xor_64`; "C6 is a **producer accelerator** … its speed-up surfaces inside W3's must-improve rows." The consumer — the W3 structural-bitmap producer chain — is landed at W3 and live by the time W4c dispatches. The EOR3 ladder is a `FEAT_SHA3`-conditional specialisation of an existing body, scalar fallback unconditional: a replacement, not an addition. Non-orphan. | ACCEPT |
| V6 | W4d (CTZ) — kernel + consumer same-commit; the V1-unplaceable candidate | §7.4: the CSSC `ctz` replaces the consumer-side `<u16>::trailing_zeros` (`rbit+clz`) at the W4a 32-byte block scanner's mask consumer. Entry gate: "W3 closed … **and** W4a closed (the 32-byte block scanner the CTZ extracts from is live). W4d is the deepest sub-wave — it needs both." V1 D10 called C7 "structurally unplaceable in the split topology" because its non-orphan condition is *both* C3 and C5 in scope. The sub-wave fold places C3 at W3, C5 at W4a, C7 at W4d *gated on both* — the double-deep dependency is now expressible and honoured. C7's consumer (the W4a mask) exists when W4d dispatches. Resolved. | ACCEPT |
| V7 | W3 keeps substrate cardinality at one — SIMD index transient, class column co-indexed on the existing tape | `skv9-p3-F-spec-draft` §6 objective: "keep the parser-event cursor stream, add a co-indexed class column on the existing offset tape at emit time, consume the SIMD index by move (Lock 1 substrate cardinality stays at one — no new tape type, no `UnionTape`)." §6 exit-gate clause 8: "Substrate cardinality stays at one — no `UnionTape`, no new `BackendShape`, no parser-owned cursor." §1 non-negotiables carry the same. The SIMD `StructuralIndex` is a transient producer; the class column is `classes: Vec<u8>` additive on the existing `OffsetTape`. Cardinality one — unchanged from the V1 D1 ACCEPT. | ACCEPT |
| V8 | No V2 wave introduces a sidecar / parallel substrate / Track-1≡Track-2 dishonesty | §1 non-negotiables: "No parallel or sidecar substrate"; "No parser-owned structural cursor or parser-owned fact slot." W3 §6 pre-blocked routes disposition REDRESS 50/51/53/60-72 individually — "the class column is the tape's own column, one producer." W1 exit-gate clause 5: "no row admits on Track 1 ≡ Track 2." `track2_independence_status` (schema field 35) is gate-consumed. The W4x sub-waves are producer rewires of existing bodies. No second producer pass, no sidecar. | ACCEPT |
| V9 | Union-substrate codec consumer slice CH5 V1 said was dropped is restored | Restored by **collapse + explicit disposition** (V2-CH5-3): the codec consumes the already-wired x4 production path, stated plainly in P3-A C4 and §7.2. The V1 fold offered this as the explicit second branch; the cohort took it and carries the REDRESS-82 differential proof. The slice is no longer silently un-scoped — it is named, dispositioned, and gated. | ACCEPT |
| V10 | V2 SPEC carries Lock-1 binding language per wave | §1 (global non-negotiables): full Lock-1 vocabulary. §6 (W3): "Lock 1 substrate cardinality stays at one — no new tape type, no `UnionTape`"; exit-gate clause 8 binds it; pre-blocked routes REDRESS 50/51/53/60-72 + the "REDRESS 92 blanket pre-block" all carried. §7.1/§7.2/§7.3/§7.4 each carry the W10b six-row maintain gate and a Section-2.1 generality scan; §7.2 carries the REDRESS-82 differential. §2.1 the generality/Lock-14 gate per wave. Dispatch-draft Non-Negotiables section repeats "Substrate cardinality stays at one across every wave." Lock-1 language is per-wave bound. | ACCEPT |
| V11 | D6 V1 (the central fault) — cascade constraint absent from the manifest | RESOLVED. `skv9-p3-F-spec-draft` §2 preamble: "P3-A §3 records the cascade-sequencing constraint and the C1..C8 dependency graph." §2.2 is a dedicated section. The manifest table places all five cascade candidates (C3, C5, C6, C7 + C8 checkasm). The constraint is no longer absent. | ACCEPT (was REJECT) |
| V12 | The collapse-to-one-consumer raises a NEW coupling question — does W4b re-body onto a *parser-owned* path? | This is the residual carried from V2-CH5-3. The codec consumer is `unescape_four_unicode_escapes` / the `Some(b'u')` arm in `parse-that-regex/src/lib.rs` — a **parser-owned string materialiser**. V1 D11 named exactly this shape ("re-bodies the codec onto the parser-owned helper — the REDRESS-82 shape") as a Lock-1-adjacent route. The V2 fold's defence is the REDRESS-82 five-axis material differential (§7.2): the SK-V9 codec is (a) a const-generic primitive with five bindings not a JSON classifier, (b) consumed at an *already-wired* x4 path not a new parser-owned site, (c) gated `parse_only`-only with a direct-route no-regression CI guard, (d) measured on post-V3 PMU evidence, (e) the 4-quartet batched path is primary. This is a genuine material differential and CHALLENGE-acceptable — BUT it is honesty-of-framing, not architecture: the consumer *is* parser-owned. The coupling is acceptable **only because the codec re-bodies a path that already exists and ships no new substrate**. Flagged as a residual risk (§4) — not a REJECT, because the differential is real and the slice is no longer a *new* parser-owned scratch buffer. | REVISE |
| V13 | D7 V1 (orphan-kernel posture — "W3 closed" ≠ "same wave") | RESOLVED. The §2.2 disambiguation is precisely that "W3 closed" (i.e. union substrate exists and is live) IS the cascade-lock satisfaction — the V1 fault was that *no kernel-into-union wiring* accompanied the entry gate. The V2 sub-waves each carry same-commit kernel-into-union wiring (V2 row above). "W3 closed" is now a correct entry gate *because* the same-commit wiring is mandated alongside it. | ACCEPT (was REJECT) |
| V14 | D8 V1 ("Each row is one wave" codified the split) | `skv9-p3-C` line 99 still reads "Each row is one wave" — but P3-C is a falsifiability-gate artefact and "row" there means a gate-table row, not a manifest wave. The binding manifest is `skv9-p3-F-spec-draft` §2, which sub-waves W4. P3-C §W4 same-wave-consumer row (line 131) now correctly names *both* consumers: the codec's `unescape_four_unicode_escapes` x4 path AND `scan_string_special_block_32`'s `match_string_at_quote_trusted_utf8`. The V1 substitution-of-a-different-consumer fault is gone. The "one wave" phrase is a stale-adjacent residual in a non-manifest artefact; cosmetic. | REVISE |
| V15 | D11 V1 (the split forces the codec into a parser-owned-helper fallback — the Lock-1 re-opening) | RESOLVED at the architecture level by V2-CH5-3: there is no longer a split that *orphans* the union-substrate codec consumer, because that slice was collapsed into the already-wired x4 path. W4b dispatches *after* W3 (union live) with W4a paired — the kernel's consumer exists. The residual — that the consumer is parser-owned — is V12, a REVISE not a REJECT. The Lock-1 *re-opening* (a new retained sidecar / parallel substrate) does not occur: §7.2 ships no substrate, only a primitive re-body. | ACCEPT (was REJECT) |
| V16 | D24 V1 (SPEC W3↔W4 dependency text reified the split into a non-negotiable-adjacent sentence) | RESOLVED. §7 preamble: "each sub-wave's redress wires its kernel into the **already-landed W3 union substrate** in the same commit (the cascade-lock, §2.2)." The "consumer base" phrasing survives in the §2 manifest table W4a cell but is now correct (the union substrate IS the same-commit consumer base) and cross-referenced to §2.2. No sentence reifies a cascade *violation*. | ACCEPT (was REJECT) |
| V17 | D25 V1 (dispatch-draft propagated the fault to execution) | RESOLVED. The dispatch-draft Wave Manifest reads "W4a … Conditional on W3 close (cascade-lock)"; the dedicated "**The cascade-lock (SPEC §2.2)**" paragraph states "It does NOT mean one monolithic wave. Each W4 sub-wave's redress commit wires its kernel into the already-landed W3 union **in the same commit**." Phase-3 protocol mandates same-commit kernel-into-union wiring. An implementation agent following this contract ships the same-commit consumer wiring, not the orphan posture. | ACCEPT (was REJECT) |
| V18 | D26 V1 (three "same-wave" relations conflated) | RESOLVED by V2-CH5-5 — §2.2 enumerates the three relations distinctly (cascade-lock / same-wave consumer / codec-scanner pairing). | ACCEPT (was REVISE) |
| V19 | W2 (P2-B proof) coupling surface — no smuggled production consumer | `skv9-p3-F-spec-draft` §5: all proof files `#[cfg(any(test, feature = "proof"))]`; "Proof-only depth: zero `RESULTS.md` row movement, zero generated output, zero production consumer." Exit-gate clause 6: `rg 'event_grammar' skinny/crates/bbnf-bench/` returns zero — the witnesses cannot be reached by `cargo bench`. `same_wave_consumer_class = gate_only` for W2 (P3-A C2: "the same-wave-consumer rule binds substrates, not contracts"). No retained substrate, no coupling. | ACCEPT |
| V20 | W1 (P2-C) coupling surface | §4: P2-C is "a mechanical baseline-whitelist expansion"; touches no Lock-1 substrate, adds no SIMD primitive. Exit-gate clause 5 forbids Track 1 ≡ Track 2; clause 7 is the Section-2.1 generality scan. `same_wave_consumer_class = gate_only`. No coupling. | ACCEPT |
| V21 | W3 `at_cursor` consumer wired same-commit; `consume_structural` deleted not renamed | §6 owner table slice A.4: `JsonNodeKind::at_cursor` byte-rediscovery → class-column read — "the same-wave production consumer for A.1." Exit-gate clause 3: "`consume_structural` is deleted from `generated.rs`; the class column read is present in `at_cursor` … CH5 falsifier: `rg 'consume_structural' skinny/crates/runtime/src/` returns zero outside the deletion-commit diff." Deletion, not rename — unchanged from V1 D14/D15 ACCEPT. | ACCEPT |
| V22 | Track-2 independence across W3/W4a-d | §6 exit-gate clause 4: "Track 2 / `path!` / direct-to-struct / SinkOnly rows show no delta beyond noise (no cross-substrate leak)." `track2_independence_status` is schema field 35, gate-consumed. The W3 class column touches only retained-view consumers; the W4x kernels re-body existing bodies. No Track-1≡Track-2 dishonesty. | ACCEPT |
| V23 | W3 class column carries only structural ordinals — no Number/Literal leak | §6 exit-gate clause 5: "The class column carries only structural ordinals the SIMD producer can fill; no `Number`/`Literal` ordinal leaks into the structural alphabet." §2.1: "The W3 class column stores opaque class ordinals." The class alphabet is bounded to the SIMD-fillable structural set — no semantic-payload smuggling into the substrate. | ACCEPT |
| V24 | W4b codec/scanner pairing — strictly adjacent, not separable | §7.2: "W4b is **PAIRED with W4a — strictly adjacent, never separable.** … W4b dispatches only with W4a landed." Exit-gate clause 3: `unicode_mixed` "Admits iff the *combined* W4a string-block + W4b codec measured Mbps clears." The pairing is the third "same-wave" relation (§2.2) and is honoured — neither sub-wave closes the four uncloseable rows alone, so the pairing prevents a single-candidate paper-close. | ACCEPT |
| V25 | W4c EOR3 — `FEAT_SHA3`-conditional, scalar fallback unconditional (no Lock-16 default-rewire) | §7.3 owner table: "The scalar shift-XOR ladder stays the unconditional fallback." Exit-gate clause 4: "a capability-conditional specialisation, the same admissibility shape as `digit_mac` (DotProd-gated)." Pre-blocked routes carry the REDRESS 88 differential and the HANDOFF §5 "PMULL prefix-XOR as default hot paths" disposition. The EOR3 path is wave-internal and host-gated — no default-hot-path rewire, no Omega dependency. | ACCEPT |
| V26 | W4d CTZ — host-capability-gated at a non-default call site, `rbit+clz` fallback | §7.4 owner table: "The `rbit + clz` form stays the unconditional fallback." Exit-gate clause 3: "The CTZ body is host-capability-gated at the non-default call site." Pre-blocked routes carry the REDRESS 89 differential and the W10b six-row maintain gate as "the hard blocking precondition." No default rewire. | ACCEPT |
| V27 | `substrate_cardinality` schema field gate-consumed at W3 | `skv9-p3-D` schema field 33 `substrate_cardinality` = `one`; §"W3" row: "`substrate_cardinality` MUST stay `one`." `skv9-p3-F-spec-draft` §0.y lists it in the 36-field set; §6 exit-gate clause 8 binds it. The 36-field schema (no 37th column) means no `union_class_column_status` invention — the substrate identity rides existing fields (V2-CH5-6 resolution). | ACCEPT |
| V28 | C8 checkasm-parity backfill — bundled precondition, not orphaned | `skv9-p3-A` §3: C8 "is not a standalone wave but a *bundled precondition* distributed across the C4 wave … and the C5 wave." `skv9-p3-F-spec-draft` §7.1/§7.2/§7.3 each name the checkasm file landing FIRST and blocking the wave (`checkasm_string_block.rs`, `checkasm_escape_codec.rs`, `checkasm_bitmap_prefix_xor_64.rs`); W4d's CTZ correctness rides W4a's `checkasm_string_block.rs`. Every kernel has a same-wave checkasm gate. No orphan-kernel-untested route. | ACCEPT |
| V29 | W3 partial-revert does not block W4 — the substrate is the dependency, not the exit-gate Mbps | §6 revert protocol: "A full W3 revert blocks W4a-d: the union substrate is the cascade-locked consumer base. A *partial* W3 (class column lands, exit rows NEAR-MISS) does not block W4 — the substrate is the dependency, not the exit-gate Mbps." This is coupling-honest: the cascade-lock binds on the *substrate existing*, not on W3's Mbps gate passing. Correct reading of P2-D §0. | ACCEPT |
| V30 | Dispatch-draft per-wave triumvirate — each W4 sub-wave a fresh triumvirate, own 75-min redress | Dispatch-draft Phase-3: "Each W4 sub-wave gets its own 75-min redress — that is the point of the sub-wave structure." `skv9-p3-F-spec-draft` §2.2: "each W4x is a fresh triumvirate, individually inside its LOC budget and its 75-min redress cap." The sub-wave structure resolves the CH4 wave-too-big finding (monolithic ~1,595-1,860 LOC) without violating the cascade — each sub-wave's kernel-into-union wiring is same-commit. | ACCEPT |

Spot-check count: 30 dispositioned rows (V1-V11 the named/V1-resolution
items, V12-V30 the surrounding coupling surface) — exceeds the ≥25 and
≥15 task minima.

## §3 — Aggregate verdict

Dispositions: 30 rows — **28 ACCEPT, 2 REVISE, 0 REJECT**.

ACCEPT rate 28/30 = **93.3%**.

This is a near-miss of the §3Z ≥95% convergence bar. The two REVISE
rows are V12 (the codec re-bodies a parser-owned materialiser path —
acceptable only on the REDRESS-82 material differential) and V14 (P3-C
line 99 "Each row is one wave" is a stale-adjacent phrase in a
non-manifest artefact). Neither is a coupling *fault*: V12 is a
framing-honesty flag on an architecturally-sound route, and V14 is
cosmetic — the binding manifest is the SPEC §2 sub-waved table, not the
P3-C gate-table phrasing.

The V1 → V2 movement is decisive. All six V1 REJECT rows
(D6/D7/D8/D11/D24/D25) are resolved to ACCEPT; the three coupling-fault
REVISE rows (D4/D20/D26) are resolved. The root cause — the cascade
split conflated across three "same-wave" relations — is closed by the
`skv9-p3-F-spec-draft` §2.2 disambiguation, which is the precise
artefact the task and the V1 consolidation item-6 demanded. The
sub-wave fold (W4a-d) honours the cascade-lock literally: each sub-wave
dispatches only after the W3 union substrate is landed and live, and
each wires its kernel into that substrate in the same redress commit.
No orphan kernel, no parser-owned-scratch *substrate*, no sidecar, no
parallel substrate, no Track-1≡Track-2 dishonesty. Substrate
cardinality stays at one at every wave.

CH5 V2 **clears the cohort on coupling** — at 93.3% it is a hair under
the formal §3Z bar, but the two residuals are a framing flag and a
cosmetic phrasing, neither re-opening Lock 1 nor a REDRESS sidecar
route. CH5's verdict is **ACCEPT-WITH-NOTE**: the two §4 items are
surgical touch-ups, not a V3 re-fold; if the orchestrator wants a clean
≥95% they are a one-commit P3-C/SPEC tidy.

## §4 — Remaining coupling risks

**N1 (REVISE, V12) — the codec consumer is a parser-owned string
materialiser; the differential is honesty-of-framing.** The V2 fold
resolved V2-CH5-3 by collapsing the codec's consumer to the
already-wired `unescape_four_unicode_escapes` x4 path in
`parse-that-regex/src/lib.rs` — a parser-owned materialiser. This is
architecturally sound (no new substrate, no new scratch buffer; the
path already exists and is already a production consumer) and CH5 does
not reject it. But the cohort must keep the REDRESS-82 five-axis
material differential (§7.2 pre-blocked routes) load-bearing through
implementation: if W4b's redress agent ever re-bodies the codec onto a
*new* parser-owned site, or adds a retained per-`\u` validator/scratch,
the differential collapses and the route becomes the REDRESS-82 shape.
The W4b CHALLENGE must re-confirm at plan time that the consumer is the
pre-existing x4 path and `sink.rs` call-site swap only — no new
parser-owned helper. This is a watch-item for the W4b triumvirate, not
a P3-artefact defect.

**N2 (REVISE, V14) — `skv9-p3-C` line 99 "Each row is one wave" is a
stale-adjacent phrase.** P3-C is a falsifiability-gate artefact; "row"
there means a gate-table row, and the binding manifest is the
sub-waved SPEC §2 table. But the phrase, read in isolation, echoes the
V1 D8 split-codifying language. A one-line P3-C edit ("Each gate-table
row corresponds to one wave or sub-wave; W4 is sub-waved per SPEC §2.2")
removes the ambiguity. Cosmetic; non-blocking.

**No structural coupling risk remains.** The P2-A union architecture
stands (unchanged since V1). The wave-sequencing correction the V1 fold
demanded is delivered: the cascade-lock is disambiguated in the SPEC,
the W4 sub-waves each wire their kernel into the already-landed W3
union same-commit, the union-substrate codec consumer slice is
dispositioned (collapsed to the already-wired x4 path, with the
REDRESS-82 differential carried), C6/C7 are placed, the three
"same-wave" relations are named distinctly, and the schema stays at 36
fields with substrate cardinality one. The two §4 items are surgical;
they do not re-open Lock 1 or any REDRESS sidecar route.
