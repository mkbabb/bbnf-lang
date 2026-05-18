# SK-V9 S-P3 CHALLENGE V1 — CH5 HIDDEN COUPLING

Lens: CH5 — HIDDEN COUPLING (`ORCHESTRATOR` §3W). Cycle: V1.
Date: 2026-05-18.
Cohort under audit: SK-V9 S-P3 — the seven P3 artefacts at
`restart/skinny/tranches/sk-v9/research/p3/` (A shortlist, B
wave-sequencing, C falsifiability-gates, D telemetry-schema, E
pre-blocked-ledger, F spec-draft, F dispatch-draft).
Disposition: ONE file; ACCEPT / REVISE / REJECT per row; no commit.

## §1 — Method

CH5 hunts the five Lock-1 failure shapes: a second retained substrate; a
sidecar producer running alongside parse; a renamed-scanner Lock-1
violation; a Track-1 ≡ Track-2 dishonesty; and the looser shape this
cohort exposes — a wave-sequencing split that *forces* an honest
substrate-bound design into a dishonest orphan-producer posture. The
substrate-cardinality invariant is Lock 1 verbatim: "A SIMD mask stream
is a transient producer, not a retained sidecar; if structural offsets
are retained, the structural projection IS the tape." `substrate_cardinality`
must read `one` at every wave (`skv9-p3-D` row 33).

Each P3 artefact was read end-to-end. The coupling-load-bearing wave is
W3 (the P2-A union event-model). The audit cross-checks W3's design
(`skv9-p2-A`) against P3-B's manifest, P3-C's gates, P3-D's schema,
P3-E's ledger, and P3-F's two drafts; and against the P2-D §0
cascade-sequencing constraint and the P2-D §3.5 / §4.4 / §5.3.1 / §5.4
"blocks on P2-A landing OR fails CH5" sentences — those four sentences
*name CH5 by name* as the failure detector, so this lens is their
adjudicator, not a bystander.

Verdict legend: ACCEPT — coupling-clean as drafted. REVISE — sound in
substance, defective in a citable particular; surgery named. REJECT —
the artefact ships a coupling fault that re-opens Lock 1 or a REDRESS
sidecar route.

The five task questions are dispositioned in §2 rows D1–D6 (the named
questions), D7–D27 (the surrounding coupling surface). The aggregate
verdict and the V2-fold list follow in §3 and §4.

## §2 — Disposition table

| # | Subject | Finding | Verdict |
|---|---|---|---|
| D1 | W3 keeps substrate cardinality at one — SIMD index transient, class column co-indexed | `skv9-p2-A` §2.1 splits the cursor stream (existing parser-event tape, unchanged count) from the class stream (a co-indexed `Vec<u8>` column on the *existing* `OffsetTape` runtime struct). The SIMD `StructuralIndex` is produced inside `parse(input)`, consumed by move by the walker, dropped at the parse-frame end; §2.1's grep falsifier (`rg 'StructuralIndex' skinny/crates/runtime/src/` matches only inside the parser body, never in `tape/`, never as a retained struct field) is correct and binding. The class column is additive on the retained tape, not a parallel substrate. Cardinality stays at one. | ACCEPT |
| D2 | No wave introduces a sidecar producer / parallel substrate / Track-1≡Track-2 dishonesty | W3: the class column is co-emitted at the *same* `emit_plain_offset` call site (`emit_event_offset(offset, class)`) by the *only* producer (the parser) — `skv9-p2-A` §6 checks REDRESS 50/51/53/60–72 individually; no second producer pass. W1: P2-C admits two measured typed rows with `serde_json::from_slice` as the independent Track-2 oracle (`skv9-p3-A` C1) — Track-1 ≢ Track-2. W4/W5: ASM kernels are producer rewires, not new substrates. No sidecar. | ACCEPT |
| D3 | P3-B's claim that W3 does NOT need the SC-6-L1-R1 Pass Omega refinement | `skv9-p3-B` §4 is correct *for the W3 union as designed*: `skv9-p2-A` §2.1/§2.2 builds the union strictly within Lock 1 as written — the index is transient, the class column interior to `OffsetTape`, no sixth `BackendShape`, no `UnionTape`. P2-B §5 confirms the asymmetry: SC-6-L1-R1 "can be ratified before, after, or independently." The union REPLACES the offset-tape's role (it *is* the offset tape, refined), it does not run alongside it. The claim is correct. **Caveat carried to D4/D11**: the claim holds only because W3 does not co-land the P2-D consumers; the moment a P2-D consumer is co-waved with W3, the union must be the consumer's *production substrate* and the no-Omega claim must be re-checked against that consumer's substrate touch — see D11. | ACCEPT |
| D4 | P2-D ASM kernels each REPLACE an existing producer, not add one | Per-kernel: C5 `scan_string_special_block_32` REPLACES the 16-byte `scan_string_special_block` at the existing `match_string_at_quote_trusted_utf8` call site (`skv9-p2-D` §4.0 — "the 32-byte block replaces the producer at the existing call site, no new wrapper"). C6 EOR3 ladder REPLACES the scalar shift-XOR ladder inside `bitmap_prefix_xor_64` as a `FEAT_SHA3`-conditional specialisation, scalar fallback retained — replacement, not addition. C7 CSSC CTZ REPLACES `rbit+clz` at the string-mask first-set extract. C4 codec NEON body re-bodies the already-wired x4 path. None ADD a producer. **But** D4 is contingent on D11: if a kernel lands in a wave WITHOUT its production consumer, the replacement-vs-addition distinction collapses — an orphan kernel IS an addition. The kernels are replacements *only inside the cascade-locked wave*. | REVISE |
| D5 | P3-F SPEC draft's per-wave sections carry the Lock-1 binding language | `skv9-p3-F-spec-draft` §1 (non-negotiables) carries the full Lock-1 vocabulary: "No `UnionTape`… Lock 1 substrate cardinality stays at one", "No parser-owned structural cursor… transient producer consumed by move", "No parallel or sidecar substrate". §6 (W3) repeats it: "Lock 1 substrate cardinality stays at one — no new tape type, no `UnionTape`"; §6 exit gate #8 is "Substrate cardinality stays at one". §0.1 close-condition is silent on cardinality but §0.4 schema row `substrate_cardinality` MUST stay `one` carries it. The W3 section is bound. | ACCEPT |
| D6 | The cascade-sequencing constraint (P2-D kernels same-wave-locked to the union) is in the wave manifest | **FAULT.** `skv9-p2-D` §0 is verbatim: "P2-A must land in the same wave as any of these P2-D consumer slices, or the slices fall back to REDRESS-rejected parser-owned shapes; **the wave may not be split**." `skv9-p3-A` §3 honours it — the dependency graph names "DEPTH 2 — same-wave with C3 (the cascade-locked block)" and instructs "P3-B should sequence C3 + C4 + C5 + C6 (+ C7) as one cascade-locked behaviour wave… not as independent W{n}." But `skv9-p3-B` §2 manifest SPLITS them: C3 → W3, C5 → W4, C4 → W5, C6/C7 unplaced. P3-B §2 contains NO mention of the cascade constraint; §3 W4/W5 dependency text says "gated on W3 close" — i.e. a *later* wave, the split P2-D §0 forbids. The constraint is absent from the manifest. This is the central CH5 fault of the cohort. | REJECT |
| D7 | P3-B W4 "entry gate: W3 closed" — the orphan-kernel posture | `skv9-p3-B` §3 W4: "Its entry gate is **W3 closed with the P2-A union substrate landed**: P2-D §3.5/§4.4 state the codec broadening and the CSSC CTZ consumer block on P2-A landing in the same wave or fail CH5." P3-B *quotes the same-wave clause* and then violates it by making W4 a wave *after* W3 closes. "W3 closed" ≠ "same wave as W3". P2-D §3.5 is explicit: "if P2-A doesn't land **simultaneously**, the codec broadening ships as a primitive without its production consumer — a REDRESS-82-style orphan." P3-B's own §3 text is self-falsifying. | REJECT |
| D8 | P3-C "Each row is one wave" | `skv9-p3-C` line 99: "Each row is one wave." This codifies the split. P3-C §W4 same-wave-consumer row names `scan_string_special_block_32`'s consumer as `match_string_at_quote_trusted_utf8` — NOT the union substrate. P2-D §3.5 names the union substrate as the codec's same-wave consumer. P3-C has substituted a *different* consumer to make the one-row-one-wave split appear consumer-honoured. The string-block widening's first consumer is indeed `match_string_at_quote_trusted_utf8` (P2-D §4.0), so C5 alone is not orphaned — but C4 (codec) and C6 (EOR3) and C7 (CTZ) ARE orphaned by the split, and P3-C never dispositions them. | REJECT |
| D9 | C6 EOR3 ladder — orphaned by the manifest | `skv9-p3-A` C6: "Depends on C3 (same-wave)… its only consumer is the §5 structural-bitmap producer (C3 scope); absent C3 in the same wave, C6 ships orphaned." `skv9-p3-B` manifest places C6 in NO wave at all — W4 is "string-block widening", W5 is "codec". C6 is unplaced; P3-F SPEC §7/§8 owner tables do not list `bitmap_prefix_xor_64.rs`. C6 is silently dropped from the manifest. A dropped candidate is not a coupling fault per se, but C6 is the SHA3 producer-accelerator for the W3 structural-bitmap chain (`skv9-p2-A` §2.3 producer vocabulary) — dropping it without an explicit "deferred to SK-V10, blocker: …" entry is a P3-E ledger gap (D17). | REVISE |
| D10 | C7 CSSC CTZ — orphaned and double-deep | `skv9-p3-A` C7: "Depends on C3 (same-wave) + C5 + C8… the string-mask consumer that makes the CTZ extract non-orphan is C3 scope." P3-B manifest does not place C7. P3-F SPEC §7 W4 owner table has no CTZ slice. C7's non-orphan condition is *both* C3's union string-mask consumer AND C5's 32-byte block scanner in the same wave. The split puts C3 in W3 and C5 in W4 — C7 can never be non-orphan under the P3-B manifest. C7 is structurally unplaceable in the split topology. | REVISE |
| D11 | The split forces the codec's union-substrate consumer into a parser-owned-helper fallback — the Lock-1 re-opening | This is the coupling consequence the four P2-D "fail CH5" sentences predict. `skv9-p2-D` §3.5: "broadening alone, in the absence of the union substrate, only reduces fall-through traffic in the **parser-owned helper**, which is the shape REDRESS 82 rejected." The codec has TWO consumer slices (`skv9-p2-D` §3.5 LOC table): the per-quartet NEON wire (consumer = the existing x4 path in `parse-that-regex`) and the union-substrate codec consumer (per-tape-cell projection, "80-150 LOC in P2-A union-substrate crate", "Co-developed with P2-A"). The union-substrate consumer cannot be co-developed with P2-A if P2-A is a *prior closed wave*. The split therefore either (a) orphans the union-substrate codec consumer or (b) re-bodies the codec onto the parser-owned helper — the REDRESS-82 shape, a parser-owned scratch route, a Lock-1-adjacent sidecar. P3-B/C/F have not seen this; W5's owner table (`skv9-p3-F-spec-draft` §8) lists only `parse-that-regex/src/lib.rs:402` and `unescape_uxxxx.rs` — the parser-owned-helper path, not the union-substrate consumer. **W5 as drafted ships the REDRESS-82 shape.** | REJECT |
| D12 | `substrate_cardinality` schema field gate-consumed at W3 | `skv9-p3-D` §4 W3 row: "`substrate_cardinality` MUST stay `one`." Row 33 of the schema table binds it to manifest scope, value `one`. The field is present, the W3 rule is stated, the gate-json non-producer rule (`skv9-p3-D` §6) makes a producer-only field a wave failure. The schema instrumentation for D1 is correct. ACCEPT for the schema; the *behavioural* cardinality of W4/W5 under the split (D11) is the unresolved item, not the field. | ACCEPT |
| D13 | `union_class_column_status` field — W3 only | `skv9-p3-F-spec-draft` §0.4 adds `union_class_column_status — present \| absent \| N/A; W3 must report the class column is co-indexed and SIMD-filled.` Correct for W3. But W4/W5 (string-block, codec) under the split also touch the parse/string loop and — if cascade-locked as P2-D demands — co-consume the class column. The field is scoped W3-only; a cascade-locked wave would need it gate-consumed at the consumer wave too. Minor; folds with D6's manifest correction. | REVISE |
| D14 | W3 `at_cursor` consumer wired same-commit — the genuine same-wave consumer | `skv9-p2-A` §2.4 #2 + §5.4: `JsonNodeKind::at_cursor` is re-bodied to read `tape.class_at(cursor)`; the byte-rediscovery line `value.rs:33-46` is DELETED in the same commit. `skv9-p3-C` §W3 same-wave-consumer row confirms it and names the CH5 falsifier (`rg 'consume_structural'… returns zero outside the deletion diff`). W3's own internal same-wave consumer is honest and present. The W3 wave in isolation is coupling-clean — the fault is the *downstream* split, not W3's interior. | ACCEPT |
| D15 | `consume_structural` deletion — renamed-scanner check | `skv9-p2-A` §2.5: `consume_structural` is DELETED, not renamed; the walker consults the moved-in index. The CH5 R-CH5-1 falsifier (`rg 'consume_structural'` returns zero in `runtime/src/` and `codegen/src/` after regen) is in §2.5 and re-cited in `skv9-p3-C` §W3. No renamed scanner; the per-byte rediscovery is excised, not relabelled. | ACCEPT |
| D16 | P3-E ledger — W3 union vs REDRESS 50/60–72 sidecar routes | `skv9-p3-E` §3.2: "the class column is not a side table — it is the tape's own column… does not reopen 60–72's sidecar producers (no second producer pass)." Correct, and it cites P2-A §6's per-REDRESS differential. The ledger's coupling disposition of W3 is sound. | ACCEPT |
| D17 | P3-E ledger — C6/C7 dropped without a deferral entry | `skv9-p3-E` does not carry a deferral entry for C6 (EOR3) or C7 (CSSC CTZ). `skv9-p3-A` §1.4 "Drops" names only AESE; C6/C7 are NOT drops in P3-A — they are cascade-locked candidates. P3-B silently omits them from the manifest (D9/D10). Per the no-paper-close discipline a candidate the manifest cannot place must carry an explicit "deferred to SK-V{n}, blocker: cascade topology" entry in P3-E. The ledger has the gap. | REVISE |
| D18 | W1 (P2-C) coupling surface | `skv9-p3-A` C1 / `skv9-p3-F-spec-draft` §4: P2-C touches no Lock-1 substrate, adds no SIMD primitive, moves a row-table only. Track-2 oracle is `serde_json::from_slice`, structurally independent (`skv9-p3-A` C1 — "P2-C §2.7"). The exit gate #5 (`skv9-p3-F-spec-draft` §4) explicitly forbids `Track 1 ≡ Track 2`. No coupling. | ACCEPT |
| D19 | W2 (P2-B) proof — no smuggled production consumer | `skv9-p3-F-spec-draft` §5: all proof files `cfg(feature = "proof")`, default build byte-identical, "zero production consumer". `skv9-p3-A` C2: "the same-wave-consumer rule binds *substrates*, not *contracts*." Exit gate #3: a Lock-1 audit confirms `ValueRef<G>` owns no parser cursor or fact slot. The proof carries no retained substrate; no coupling. The proof's admissibility *rests* on having no production consumer and the SPEC gate enforces it. | ACCEPT |
| D20 | P2-D §3 codec — the two-slice consumer ambiguity | `skv9-p2-D` §3.5 LOC table separates "per-quartet NEON fallback wire" (consumer: the existing x4 path) from "union-substrate codec consumer" (consumer: P2-A union substrate, "co-developed with P2-A"). `skv9-p3-A` C4 collapses both into "re-bodies an existing production path" and names only `parse-that-regex/src/lib.rs:402`. The union-substrate codec consumer slice is dropped from C4's owner paths. Either the codec ships without its union-substrate consumer (orphan) or the union-substrate consumer is silently un-scoped. The cohort has lost a P2-D-named slice. | REVISE |
| D21 | W4 string-block — `match_string_at_quote` is a genuine consumer | `skv9-p2-D` §4.0 names `match_string_at_quote_trusted_utf8` as the *first* consumer of `scan_string_special_block_32`. This consumer is real and pre-existing, so C5 *in isolation* is not orphaned — the 32-byte body replaces the 16-byte body at a live call site (D4). C5 is the one P2-D candidate whose primary consumer is NOT the union substrate. P3-C §W4 is correct to name it. The fault is that P3-B/C/F treat C5's independence as licence to split the *whole* P2-D block — C4/C6/C7 do not share C5's independence. | REVISE |
| D22 | W4 CSSC CTZ sub-slice — named in P2-D §4.4, absent in P3-F W4 owner table | `skv9-p2-D` §4.4 makes the CSSC CTZ a sub-slice of the string-block wave that "blocks on P2-A landing." `skv9-p3-F-spec-draft` §7 W4 owner table lists `string_block.rs`, `checkasm_string_block.rs`, `lib.rs:162`, bench guard — no CTZ slice. C7 is neither in W4 nor anywhere. The SPEC draft has silently shed a P2-D §4.4 slice that P2-D explicitly bound. | REVISE |
| D23 | W5 codec — `codec_admission_basis` honesty (not a coupling fault, noted for completeness) | `skv9-p3-F-spec-draft` §8 + `skv9-p3-D` `codec_admission_basis` field: zero rows admit on the codec alone; conditional-same-wave admission honestly recorded. This is CH6 territory; CH5 notes only that the "same-wave" in `codec_admission_basis` refers to W5-internal pairing of codec+widening, which is itself a *third* re-use of "same-wave" distinct from the P2-D §0 cascade "same-wave" (codec+union). The vocabulary collision (D26) obscures the D11 fault. | REVISE |
| D24 | P3-F SPEC W3↔W4 dependency text | `skv9-p3-F-spec-draft` §7 W4: "The W3 union is the consumer base — W4 cannot dispatch before W3 closes." This phrasing — "consumer base", "before W3 closes" — encodes the split as architecture. It directly contradicts `skv9-p2-D` §0 "the wave may not be split". The SPEC draft has reified the fault into a non-negotiable-adjacent sentence. | REJECT |
| D25 | P3-F dispatch-draft W4/W5 dispatch status | `skv9-p3-F-dispatch-draft` Wave Manifest: "W4 … Conditional on W3 close", "W5 … Conditional on W4 close". The dispatch contract instructs the implementation triumvirate to treat W4/W5 as post-W3 waves. An implementation agent following this contract will ship the orphan-kernel / parser-owned-helper shape (D11). The dispatch draft propagates the fault to execution. | REJECT |
| D26 | "same-wave" vocabulary overload across the cohort | The cohort uses "same-wave" for three distinct relations: (1) P2-D §0 cascade — C4/C5/C6/C7 same wave as C3; (2) the per-kernel same-wave-consumer rule — kernel + its consumer in one commit; (3) P2-E §6.3/§6.4 — codec + string-widening paired for `unicode_mixed` admission. P3-B/C/F satisfy (2) and (3) and silently drop (1). The overload let the cohort believe the cascade was honoured because *a* same-wave rule was. The three relations must be named distinctly in V2. | REVISE |
| D27 | Track-2 independence across W3/W4/W5 | `skv9-p2-A` §4.4 #4 falsifier: "Track 2 / `path!` / direct-to-struct / SinkOnly rows show no delta beyond noise" — a cross-substrate-leak detector, carried into `skv9-p3-C` §W3 maintain envelope and `skv9-p3-D` `track2_independence_status` (value `independent_verified`). The W3 class column touches only retained-view consumers; Track 2 is structurally untouched. No Track-1≡Track-2 dishonesty in the union design. The field and the falsifier are correctly placed. | ACCEPT |

## §3 — Aggregate verdict

Dispositions: 27 rows — **12 ACCEPT, 9 REVISE, 6 REJECT**.

ACCEPT rate 12/27 = 44%. This is **far below** the §3Z ≥95% convergence
bar. CH5 returns the cohort to S-P3 for a V2 fold.

The cohort is **bimodal**. The P2-A union event-model itself (W3 in
isolation — D1, D5, D14, D15, D16, D27) is coupling-clean: substrate
cardinality stays at one, the SIMD index is a genuine transient
move-consumed producer, the class column is an additive column on the
existing `OffsetTape` runtime struct, the `at_cursor` same-wave consumer
is wired in the same commit, `consume_structural` is deleted not
renamed, and no Pass Omega SC-6-L1-R1 refinement is needed (D3). The
five named task questions §1–§5 (D1, D2, D3, D4, D5) each disposition
ACCEPT or REVISE-with-caveat — the union *as designed by P2-A* survives
CH5.

The cohort **fails on question 6 (D6) and its consequences**. P2-D §0
states a verbatim, load-bearing constraint: P2-A must land in the SAME
WAVE as the P2-D consumer slices, "the wave may not be split". P3-A §3
correctly transcribes it and instructs P3-B to sequence C3+C4+C5+C6+C7
as one cascade-locked wave. P3-B §2 then **splits them anyway** — C3 to
W3, C5 to W4, C4 to W5, C6/C7 nowhere — with no mention of the
constraint. P3-C codifies the split ("Each row is one wave"). P3-F's
SPEC and dispatch drafts reify it into entry-gate language ("W4 cannot
dispatch before W3 closes"). The split is propagated through five of
the seven artefacts and into the implementation contract.

The coupling consequence is not cosmetic. P2-D §3.5 predicts it
verbatim and names CH5 as its detector: absent P2-A in the same wave,
the codec broadening "only reduces fall-through traffic in the
**parser-owned helper, which is the shape REDRESS 82 rejected**." W5 as
drafted (`skv9-p3-F-spec-draft` §8) lists exactly the parser-owned-helper
owner paths and omits the "union-substrate codec consumer" slice that
P2-D §3.5 explicitly scoped. **W5 as drafted ships a REDRESS-82
parser-owned-scratch shape** — a Lock-1-adjacent sidecar route the
cohort believes it has avoided because it satisfied a *different*
same-wave rule (D26 vocabulary overload).

The union design is sound. The wave plan that lands it is not. The
cohort cannot converge until the manifest is re-folded to honour the
cascade.

## §4 — Coupling risks requiring V2 fold

**V2-CH5-1 (REJECT, blocking) — re-merge the cascade-locked block.**
`skv9-p3-B` §2 manifest: collapse W3+W4+W5 into a single cascade-locked
behaviour wave (call it W3 "Union event-model + cascade-locked ASM
consumers") carrying C3 + C4 + C5 + C6 + C7. P2-D §0 "the wave may not
be split" is non-negotiable. The W10b six-row maintain gate and the
checkasm preconditions ride the merged wave. If the merged wave's LOC
exceeds the budget, that is a budget-split conversation for S-P3/P3-C —
NOT licence to split the *behaviour* across waves. C5 alone (D21) has an
independent consumer and *could* in principle precede, but C4/C6/C7
cannot; the simplest coupling-honest manifest merges all five.

**V2-CH5-2 (REJECT, blocking) — strike the post-W3 entry-gate
language.** `skv9-p3-F-spec-draft` §7 ("W4 cannot dispatch before W3
closes", "the W3 union is the consumer base") and `skv9-p3-F-dispatch-draft`
Wave Manifest ("W4 Conditional on W3 close", "W5 Conditional on W4
close") must be rewritten to the cascade-locked-single-wave shape. The
dispatch contract currently instructs the implementation triumvirate to
ship the orphan posture.

**V2-CH5-3 (REJECT, blocking) — restore the union-substrate codec
consumer slice.** `skv9-p3-A` C4 and `skv9-p3-F-spec-draft` §8 dropped
the "union-substrate codec consumer (per-tape-cell projection)" slice
that `skv9-p2-D` §3.5 LOC table explicitly scopes ("80-150 LOC… co-developed
with P2-A"). Either restore it as a W3-merged owner-path slice, or — if
the cohort means the codec to consume *only* the parser-owned x4 path —
state that explicitly and carry the REDRESS-82 differential proof, which
the cohort currently does not. The silent drop is the D11 fault.

**V2-CH5-4 (REVISE) — place or defer C6 and C7 explicitly.** `skv9-p3-B`
manifest places neither the SHA3 EOR3 ladder (C6) nor the CSSC CTZ
extract (C7). On the merged-wave fold they land inside W3. If S-P3
instead defers them, `skv9-p3-E` must carry an explicit deferral entry
naming the receiving tranche (SK-V10) and the blocker. A cascade-locked
candidate may not vanish from the manifest without a ledger entry.

**V2-CH5-5 (REVISE) — name the three "same-wave" relations distinctly.**
The vocabulary overload (D26) is what let the cohort drop the cascade
while believing it satisfied "same-wave". V2 must use three distinct
terms: *cascade-locked* (P2-D §0 — union + ASM consumers in one wave),
*same-commit consumer* (the per-kernel no-orphan rule), and *paired
admission* (P2-E §6.3 — codec+widening for `unicode_mixed`). With the
terms separated the D6 fault becomes self-evident.

**V2-CH5-6 (REVISE) — scope `union_class_column_status` to every
class-column consumer wave.** On the merged-wave fold this is moot (one
wave). If any class-column consumer is sequenced apart, the schema field
(`skv9-p3-D` §0.4 / `skv9-p3-F-spec-draft` §0.4) must be gate-consumed at
the consumer wave too, not W3-only.

**V2-CH5-7 (REVISE) — re-check the no-Omega claim against the merged
wave.** `skv9-p3-B` §4's "W3 needs no SC-6-L1-R1" is correct for the
union in isolation (D3). On the merged-wave fold the union becomes the
production substrate for the P2-D per-tape-cell codec consumer; P3-B §4
must re-state the no-Omega claim against *that* consumer's substrate
touch and confirm the per-tape-cell projection introduces no new
substrate surface. P2-A §3.1's CSS-L4 cross-grammar section suggests it
does not, but the merged-wave claim must be made explicitly, not
inherited from the isolated-W3 claim.

**Non-blocking note.** No fold is required for W1, W2, or the W3 union's
*interior* — D1/D5/D14/D15/D16/D18/D19/D27 are clean. The V2 fold is
entirely a wave-sequencing correction; the P2-A architecture stands.
