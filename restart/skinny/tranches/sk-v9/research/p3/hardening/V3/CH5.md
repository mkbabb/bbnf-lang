# CH5 HIDDEN COUPLING — SK-V9 S-P3 Synthesis-Plan, Cycle V3 (verify)

Lens: CH5 — HIDDEN COUPLING (`ORCHESTRATOR.md` §3W). Pass: S-P3
Synthesis-Plan. Cycle: V3 (verify the V2→V3 comprehensive integration
fold). Date: 2026-05-18.
Cohort under audit: SK-V9 S-P3 — the seven P3 artefacts at
`restart/skinny/tranches/sk-v9/research/p3/` after the V3 fold (all
seven stamped `Cycle: V3`, each carrying a `§0 V3 fold footer`).
Authority cross-checked: `restart/locks/LOCKS.md` Lock 1 (`:34` — "A
SIMD mask stream is a transient producer, not a retained sidecar; if
structural offsets are retained, the structural projection IS the
tape"); `skinny/REDRESS.md` entries 82, 88, 89, 92;
`HARDENING-S-P3-V2-CONSOLIDATED.md`; `V2/CH5.md`.

CH5 V2 verdict was ACCEPT-WITH-NOTE at 93.3% — 28 ACCEPT / 2 REVISE /
0 REJECT, one disposition short of the §3Z 95% bar. The two REVISEs
were V12 (the codec re-bodies a parser-owned `unescape_*` materialiser
— acceptable on the REDRESS-82 material differential, flagged as
honesty-of-framing) and V14 (P3-C line 99 "Each row is one wave" — a
stale-adjacent phrase in a non-manifest artefact). CH5 V3 verifies the
V3 comprehensive integration fold folded both residuals and that the
W4b three-way sub-division the V3 fold introduced wires every codec
sub-wave into the W3 union without an orphan, a sidecar, or a
parser-owned-scratch shape.

Verdict legend: **ACCEPT** — coupling-clean. **REVISE** — sound,
defective in a citable particular. **REJECT** — re-opens Lock 1 or a
REDRESS sidecar route.

---

## §1 — V2-residual resolution

The V2 CONSOLIDATED dispatched ONE comprehensive V3-fold integration
agent. Two CH5 residuals were carried into V3 (V2 §4 items N1/N2) plus
the four CONSOLIDATED binding items. Each is verified against the V3
artefacts.

**V2 residual N1 (REVISE V12) — the codec consumer is a parser-owned
string materialiser; the differential is honesty-of-framing.**
ADDRESSED — re-confirmed coupling-clean, not patched away. The V3 fold
sub-divided W4b but kept the consumer identity exactly: SPEC §7.2.2
owner table binds the W4b-2 consumer to "Re-body the already-wired
`unescape_four_unicode_escapes` x4 path + the `Some(b'u')` arm onto the
kernel — the production consumer" plus the `runtime/src/grammars/json/sink.rs`
call-site swap. P3-C §2a W4b-2 same-wave-consumer row reads "One
production consumer — the already-wired x4 JSON path
`unescape_four_unicode_escapes` (`parse-that-regex/src/lib.rs:402`),
re-bodied onto the `escape_codec_hex_unit` fixed-width kernel, plus the
`runtime/src/grammars/json/sink.rs` call-site swap." The REDRESS-82
five-axis differential is carried verbatim in SPEC §7.2.2 "Pre-blocked
routes" and P3-E §3.4. The consumer *is* parser-owned — the differential
is real (it re-bodies a path that already exists and ships no new
substrate), CHALLENGE-acceptable, and CH5 does not reject it. The V3
fold did not regress it; it remains a REVISE-grade framing-honesty flag,
carried to §4 N1 as a W4b-2 watch-item — see §2 V12.

**V2 residual N2 (REVISE V14) — `skv9-p3-C` "Each row is one wave"
stale-adjacent phrase.** FOLDED. P3-C §2 preamble (`:114`) now reads
"Each row is one wave. Mbps figures are `SK-V9-open` parse_only Track 1
unless flagged typed" — and §2a is a separate "W4 sub-wave gate table"
whose preamble reads "W4 is the substrate-consumer bracket, sub-waved
into W4a, the three codec sub-waves W4b-1/W4b-2/W4b-3, W4c, and W4d
(P3-F SPEC §2, §7). Each sub-wave carries its own falsifiability gate."
The §2 "Each row is one wave" sentence now sits above a table that no
longer contains a "W4 — PAIRED" or "W5 — ASM kernels" row (P3-C §1.4
and the `§0 V3 fold footer` item (3) record §2a replaced them); the
W4 sub-waves live in §2a where every row is one sub-wave. The phrase is
no longer stale-adjacent — §2 rows are W1/W2/W3, §2a rows are the six
W4 sub-waves, and each table row genuinely corresponds to one wave or
sub-wave. The V2 N2 cosmetic residual is folded — see §2 V14.

**CONSOLIDATED item 1 — re-author P3-C/D/E to the unified manifest.**
RESOLVED. All seven artefacts read `Cycle: V3`; P3-C §1.4 candidate→
wave map and §2a per-sub-wave gate table, P3-D §2.3 per-wave population
table, and P3-E §1 lettered→numeric mapping all carry the sub-waved
manifest. The V2 defect "the SPEC is correct, the siblings lag it" is
closed (CH5-relevant detail in §2 V11).

**CONSOLIDATED item 2 — sub-divide W4b.** RESOLVED. SPEC §2.2 + §7.2
cut W4b into W4b-1/W4b-2/W4b-3 along the P2-E §7.4 slice seams. The
cascade-lock and the W4a+codec pairing survive the split — CH5's
central verification target — see §2 V2-V6, V12, V24.

**CONSOLIDATED items 3/4 — W3 cap check + arithmetic.** RESOLVED. SPEC
§2.2 + §6 record W3 is not sub-waved and carries the ≤110-min
CHALLENGE-gated extension. CH5-relevant: the W3 cap decision raises a
coupling question (does not sub-waving W3 introduce a coupling problem?)
— answered in §2 V8.

All V2 residuals and CONSOLIDATED items resolve. The two divergences
from the V1 fold (sub-wave fold not monolithic re-merge; collapse-to-one-
consumer not restore-the-second-slice) are unchanged from V2 and remain
coupling-honest. The V3 fold's new structural change — the W4b three-way
split — is verified clean below.

---

## §2 — V3 disposition table

| # | Subject | Finding | Verdict |
|---|---|---|---|
| V1 | Cascade-lock disambiguation survives the V3 artefacts — a kernel may not land WITHOUT the union; W3 ≺ W4 | SPEC §2.2 ("The Cascade-Lock, Disambiguated") quotes P2-D §0 then states the binding reading in a blockquote: "The constraint means a P2-D kernel must not land **without the union substrate existing**. It does NOT mean one monolithic redress wave." It is "satisfied by **W3 (the union event-model) preceding W4a-d**." The V3 fold preserved §2.2 verbatim and extended the closing pairing sentence to "W4a pairs with **W4b-2**." P3-A §3, P3-B §3, P3-C §1.4, P3-E §1 all carry the disambiguated reading. The cascade-lock disambiguation survives the V3 fold across all seven artefacts. | ACCEPT |
| V2 | Each W4 sub-wave wires its kernel into the already-landed W3 union same-commit — no orphan | SPEC §7 intro: "each W4 sub-wave's redress wires its kernel into the **already-landed W3 union substrate** in the same commit (the cascade-lock, §2.2)." §2.2: "Each W4 sub-wave's redress commit then wires its kernel into that already-landed W3 union **in the same commit**: the consumer exists, the caller is wired same-commit, no orphan ships." The dispatch §"Per-Wave Triumvirate Protocol" Phase 3: "For a W4 sub-wave it wires the kernel into the already-landed W3 union substrate in the same commit." The same-commit kernel-into-union wiring is bound at three sites; the V3 fold carried it across the W4b three-way split. | ACCEPT |
| V3 | W4a (32-byte string-block) — kernel + consumer same-commit | SPEC §7.1 owner table: `scan_string_special_block_32` in `bbnf-simd/src/aarch64/string_block.rs`; the consumer is `match_string_at_quote_trusted_utf8` (`parse-that-regex/src/lib.rs:162`) — "producer-site rewire … the same-wave consumer." `G-W4a-STRING-BLOCK` clause 3: "`match_string_at_quote_trusted_utf8` is rewired to the 32-byte block in the same commit; the consumer call shows in the `samply` symbol path." A pre-existing live call site — replacement-not-addition, genuinely non-orphan. | ACCEPT |
| V4 | W4b-1 (codec scalar reference + checkasm harness) — kernel + consumer same-commit; no orphan | SPEC §7.2.1 owner table: `escape_codec/scalar.rs` (the parity oracle re-homed from `read_hex_unit_scalar` + `hex_nibble`), `escape_codec/mod.rs` (the const-generic kernel surface + dispatcher), `checkasm_escape_codec.rs` (the differential harness). `G-W4b-1-CODEC-HARNESS` clause 2: "the harness IS the same-wave consumer for the scalar body (the test consumes the reference)." W4b-1 ships no NEON body and moves no row — it is the parity foundation the SK-V5 orphan-kernel discipline mandates before any codec body wires. Its same-wave consumer is the checkasm test; the const-generic surface is consumed by W4b-2/W4b-3 (later sub-waves, not same-commit) — correctly so, because W4b-1 ships no kernel needing a hot-path consumer, only a reference + a harness. No orphan. | ACCEPT |
| V5 | W4b-2 (fixed-width codec bodies + JSON consumer) — kernel + consumer same-commit; no parser-owned-scratch REDRESS-82 shape | SPEC §7.2.2 owner table: kernels `hex_x4_neon.rs`/`hex_x8_neon.rs`/`surrogate_join.rs` in `bbnf-simd/src/aarch64/escape_codec/`; consumer is the re-body of the already-wired x4 path at `parse-that-regex/src/lib.rs:402` + the `Some(b'u')` arm at `:718-810` + the `runtime/src/grammars/json/sink.rs` call-site swap. The W4b-2 redress "wires the kernel into the already-landed W3 union substrate in the same commit" (dispatch Phase 3) — and W4b-2's entry gate requires W4b-1 closed (checkasm harness live) **and** W4a closed (the paired scanner). REDRESS-82's parser-owned per-quartet classifier is **not** re-introduced: §7.2.2 "Pre-blocked routes" states the 4-quartet batched path is the union-substrate path and the single-quartet binding fires only on pre-filter reject. The kernel re-bodies an existing production path; it ships no new substrate and no new parser-owned scratch buffer. The residual — the consumer is parser-owned — is V12. No parser-owned-scratch *substrate*. | ACCEPT |
| V6 | W4b-3 (variable-width const-generic bindings + codegen) — kernel + consumer same-commit; no orphan | SPEC §7.2.3 owner table: `hex_variable_neon.rs` (CSS L4 `\HHHHHH`, JS `\u{}`), `codegen/src/escape_codec/` const-generic emission, the CSS L4 `#[cfg(test)]` scaffold. `G-W4b-3-CODEC-BINDINGS` clause 2: "the codegen template is the same-wave consumer for the const-generic surface." W4b-3 moves no row; the variable-width body has no JSON production consumer and the CSS L4 binding is a compile-validated scaffold (no production parse loop). Its same-wave consumer is the codegen template — correct: W4b-3 completes the codec's grammar-neutrality breadth, it does not land a JSON hot-path kernel. The codegen-template-as-consumer is honest — the const-generic emission IS consumed by the template, same commit. No orphan. | ACCEPT |
| V7 | W4c (EOR3) — kernel + consumer same-commit | SPEC §7.3: the `veor3q_u8` ladder replaces the scalar shift-XOR ladder *inside* `bitmap_prefix_xor_64`; "C6 is a **producer accelerator** — it moves no row of its own; its speed-up surfaces inside W3's must-improve rows." Entry gate: "W3 closed … the structural-bitmap producer (the EOR3 ladder's only consumer) is live." The EOR3 ladder is a `FEAT_SHA3`-conditional specialisation of an existing body, scalar fallback unconditional — a replacement, not an addition. The consumer (the W3 structural-bitmap producer chain) is landed at W3 and live when W4c dispatches. Non-orphan. | ACCEPT |
| V8 | W4d (CTZ) — kernel + consumer same-commit; double-deep dependency placed | SPEC §7.4: the CSSC `ctz` replaces the consumer-side `<u16>::trailing_zeros` (`rbit+clz`) at the W4a 32-byte block scanner's mask consumer. Entry gate: "W3 closed (the union-substrate string-mask consumer is the non-orphan condition) **and** W4a closed (the 32-byte block scanner the CTZ extracts from is live). W4d is the deepest sub-wave — it needs both." C7's non-orphan condition is *both* C3 and C5 in scope; the sub-wave fold places C3 at W3, C5 at W4a, C7 at W4d gated on both. The double-deep dependency is expressible and honoured; C7's consumer (the W4a mask) exists when W4d dispatches. No orphan. | ACCEPT |
| V9 | W3 keeps substrate cardinality at one | SPEC §6 objective: "keep the parser-event cursor stream, add a co-indexed class column on the existing offset tape at emit time, consume the SIMD index by move (Lock 1 substrate cardinality stays at one — no new tape type, no `UnionTape`)." `G-W3-UNION-SUBSTRATE` clause 8: "Substrate cardinality stays at one — no `UnionTape`, no new `BackendShape`, no parser-owned cursor." §1 non-negotiables carry the same. P3-D schema field 33 `substrate_cardinality` MUST stay `one` (§2.3 W3 row). The SIMD `StructuralIndex` is a transient producer; the class column is `classes: Vec<u8>` additive on the existing offset tape. This is Lock 1 (`LOCKS.md:34`) verbatim — "A SIMD mask stream is a transient producer, not a retained sidecar." Cardinality one — unchanged by the V3 fold. | ACCEPT |
| V10 | W3 cap decision (not sub-waved; ≤110-min CHALLENGE-gated) introduces no coupling problem | SPEC §2.2 "W3 redress cap" + §6 preamble: W3 is **not** sub-waved precisely because "the union substrate (A.1-A.5) and the SIMD structural-bitmap producer (A.6-A.8 + P2-D §5) form one cascade — splitting them orphans the class column from its only producer for the duration of the gap, and the SPEC §1 same-wave-consumer non-negotiable forbids that." This is the coupling-correct decision: a W3a/W3b split that landed the class column in W3a and its SIMD producer in W3b would be the orphan-substrate / consumer-later shape Lock 1 and REDRESS 92 both forbid. The ≤110-min extension is a triumvirate-timing instrument, not a substrate-cardinality or wiring change — it introduces no coupling problem. The decision *not* to sub-wave W3 is the one that *avoids* a coupling fault. | ACCEPT |
| V11 | Unified manifest across all seven artefacts — no coupling-relevant divergence | The V3 fold re-authored P3-C/D/E to the SPEC §2 manifest. CH5-relevant: a sibling artefact carrying a stale wave shape could mis-describe a kernel-into-union wiring (the V1 fault was a sibling-vs-SPEC divergence that dropped the cascade). P3-C §2a, P3-D §2.3, P3-E §1, P3-A §3 reading-the-graph, P3-B §3 W4 prose all now describe the same W4a / W4b-1/W4b-2/W4b-3 / W4c / W4d wiring with the same cascade-lock reading. No artefact describes a coupling shape the SPEC does not. The manifest is unified; the V1 sibling-divergence coupling risk is closed. | ACCEPT |
| V12 | W4b-2 re-bodies onto a parser-owned materialiser path — V2 N1 residual, re-verified across the W4b split | The codec consumer is `unescape_four_unicode_escapes` / the `Some(b'u')` arm in `parse-that-regex/src/lib.rs` — a parser-owned string materialiser. The V3 fold sub-divided W4b but kept the consumer identity exactly (SPEC §7.2.2 owner table). The defence is the REDRESS-82 five-axis material differential (§7.2.2): the codec is (a) a const-generic primitive with five bindings not a JSON classifier, (b) consumed at an *already-wired* x4 path not a new parser-owned site, (c) gated `parse_only`-only with a direct-route no-regression guard, (d) measured on post-V3 PMU evidence, (e) the 4-quartet batched path is primary. This is a genuine material differential and CHALLENGE-acceptable — BUT it is honesty-of-framing, not architecture: the consumer *is* parser-owned. The coupling is acceptable only because the codec re-bodies a path that already exists and ships no new substrate. The W4b three-way split did not change this — it isolated the row-moving consumer at W4b-2 (W4b-1/W4b-3 ship no JSON consumer), which if anything sharpens the audit surface. Flagged as a residual watch-item (§4 N1) — not a REJECT, because the differential is real and the slice is no new parser-owned scratch buffer. | REVISE |
| V13 | W4b-1/W4b-3 carry no JSON production consumer — not orphan kernels | The W4b three-way split raises a new question: are W4b-1 and W4b-3 orphan kernels (a primitive with no hot-path consumer)? They are not. W4b-1 ships *no kernel* — only a scalar reference (`scalar.rs`) and a const-generic *surface* (`mod.rs`); its same-wave consumer is the `checkasm_escape_codec.rs` harness (the test consumes the reference). W4b-3 ships a variable-width NEON body (`hex_variable_neon.rs`) whose same-wave consumer is the `codegen/src/escape_codec/` template — the const-generic emission IS template-consumed in the same commit, and the CSS L4 scaffold is a compile-validated binding witness, not a no-consumer kernel. Neither sub-wave is the SK-V5 orphan-kernel shape: W4b-1 has no kernel to orphan, W4b-3's kernel is template-consumed same-commit. The split did not manufacture an orphan. | ACCEPT |
| V14 | P3-C "Each row is one wave" — V2 N2 residual, folded | P3-C §2 preamble (`:114`) "Each row is one wave" now sits above a §2 gate table containing only W1/W2/W3 rows; the W4 sub-waves moved to a separate §2a "W4 sub-wave gate table" whose preamble reads "W4 is the substrate-consumer bracket, sub-waved into W4a, the three codec sub-waves W4b-1/W4b-2/W4b-3, W4c, and W4d … Each sub-wave carries its own falsifiability gate." P3-C `§0 V3 fold footer` item (3) records §2a "replac[es] the old §2 'W4 — PAIRED' and 'W5 — ASM kernels' gate rows." The V2 D8-echoing ambiguity ("one wave" read as forbidding the sub-wave fold) is gone — §2 rows are genuinely one-wave-each (W1/W2/W3) and §2a rows are genuinely one-sub-wave-each. The binding manifest is SPEC §2; P3-C is consistent with it. The V2 N2 cosmetic residual is folded. | ACCEPT (was REVISE) |
| V15 | No V3 wave introduces a sidecar / parallel substrate | SPEC §1 non-negotiables: "No `UnionTape`"; "No new substrate surface; no public substrate API"; "No parser-owned structural cursor or parser-owned fact slot"; "No parallel or sidecar substrate." §6 (W3) "Pre-blocked routes" dispositions REDRESS 50/51/53/60-72 individually — "the class column is the tape's own column, one producer." The W4b three sub-waves are producer rewires of existing bodies (W4b-2 re-bodies x4; W4b-1 a reference + surface; W4b-3 a variable-width body + codegen). No sub-wave ships a second producer pass or a retained second copy. P3-E §4 item 6 carries the sidecar/parallel-substrate hard pre-block (REDRESS 50/51/53/60-72/92). No sidecar, no parallel substrate. | ACCEPT |
| V16 | No V3 wave introduces a Track-1 ≡ Track-2 dishonesty | SPEC §4 (W1) exit gate clause 5: "no row admits on Track 1 ≡ Track 2." P3-D schema field 35 `track2_independence_status = independent_verified` is gate-consumed (§2.3). SPEC §6 (W3) exit gate clause 4: "Track 2 / `path!` / direct-to-struct / SinkOnly rows show no delta beyond noise (no cross-substrate leak)." The W4 sub-waves are kernel rewires measured against sonic-strict floors, not against a hand Track 2. No Track-1≡Track-2 substitution. | ACCEPT |
| V17 | W4b codec/scanner pairing — W4a + W4b-2 strictly adjacent, survives the W4b split | SPEC §2.2 closing: "The W4a + W4b pairing is preserved exactly: W4a pairs with **W4b-2** — the codec moves no row to GO without the string-block widening, and W4b-2 is the codec sub-wave that carries the row-moving consumer (P2-E §6.4). W4b-1 and W4b-3 carry no row gate." §7.2.2 header + entry gate: "PAIRED with W4a — strictly adjacent, never separable … W4a closed (the paired scanner widening is live)." `G-W4b-2-CODEC` clause 3: `unicode_mixed` "Admits iff the *combined* W4a string-block + W4b-2 codec measured Mbps clears." The pairing is the third "same-wave" relation (§2.2 enumeration) and is honoured — the W4b three-way split correctly re-pinned the pairing to W4b-2 (the row-moving sub-wave), not to the parity-foundation W4b-1 or the breadth W4b-3. Neither sub-wave closes the four uncloseable rows alone; the pairing prevents a single-sub-wave paper-close. | ACCEPT |
| V18 | The three "same-wave" relations stay named distinctly after the W4b split | SPEC §2.2 closes with the three-relation enumeration: (1) cascade-lock (a P2-D kernel lands only after the W3 union substrate exists); (2) same-wave consumer (every primitive + its hot-path caller in one commit); (3) codec/scanner pairing (W4a + W4b-2 strictly adjacent). The V3 fold updated relation (3) from "W4a and W4b" to "W4a and W4b-2" — the pairing is now pinned to the specific row-moving codec sub-wave. The vocabulary stays disambiguated; the W4b split did not re-conflate the relations. | ACCEPT |
| V19 | W4b bundle→three-sub-wave split — no pre-block / coupling-constraint dropped | P3-E §2.5 (W-UC) carries the codec honour-set and the §3.4/§3.5 adjacents; the V3 fold distributes W-UC across SPEC §7.1 (W4a — REDRESS 83 + 60-62), §7.2.1 (W4b-1 codec surface), §7.2.2 (W4b-2 — REDRESS 82 + 64 + 66-69/93), §7.2.3 (W4b-3 — REDRESS 82 variable-width + 85-87/Lock 14). The union of the four SPEC sub-wave pre-block lists ⊇ P3-E §2.5's set. CH5-relevant: no kernel-into-union wiring constraint and no pairing constraint is lost in the four-way split — W4b-2 carries the pairing, W4b-1 carries the parity-foundation precondition, W4b-3 carries the codegen consumer. The split distributes constraints; it drops none. | ACCEPT |
| V20 | W2 (P2-B proof) coupling surface — no smuggled production consumer | SPEC §5: all proof files `#[cfg(any(test, feature = "proof"))]`; "Proof-only depth: zero `RESULTS.md` row movement, zero generated output, zero production consumer." Exit gate clause 6: `rg 'event_grammar' skinny/crates/bbnf-bench/` returns zero — the witnesses cannot be reached by `cargo bench`. P3-C §3.2 carries the same `rg` falsifier. No retained substrate, no coupling. | ACCEPT |
| V21 | W3 `at_cursor` consumer wired same-commit; `consume_structural` deleted not renamed | SPEC §6 owner table slice A.4: `JsonNodeKind::at_cursor` byte-rediscovery → class-column read — "the same-wave production consumer for A.1." `G-W3-UNION-SUBSTRATE` clause 3: "`consume_structural` is deleted from `generated.rs`; the class column read is present in `at_cursor`. CH5 falsifier: `rg 'consume_structural' skinny/crates/runtime/src/` returns zero outside the deletion-commit diff." Deletion, not rename. Unchanged by the V3 fold. | ACCEPT |
| V22 | W3 class column carries only structural ordinals — no semantic-payload leak | SPEC §6 exit gate clause 5: "The class column carries only structural ordinals the SIMD producer can fill; no `Number`/`Literal` ordinal leaks into the structural alphabet." §2.1: "The W3 class column stores opaque class ordinals." The class alphabet is bounded to the SIMD-fillable structural set — no semantic-payload smuggling into the substrate. P3-D §2.3 W3 row binds `structural_projection_status` / `substrate_surface`. No leak. | ACCEPT |
| V23 | W3 partial-revert does not block W4 — the substrate is the dependency, not the exit-gate Mbps | SPEC §6 revert protocol: "A full W3 revert blocks W4a-d: the union substrate is the cascade-locked consumer base. A *partial* W3 (class column lands, exit rows NEAR-MISS) does not block W4 — the substrate is the dependency, not the exit-gate Mbps." Coupling-honest: the cascade-lock binds on the *substrate existing*, not on W3's Mbps gate passing. The V3 fold carried this verbatim; it correctly reads P2-D §0. | ACCEPT |
| V24 | W4b-2 is the only codec sub-wave with a row gate — the pairing cannot be paper-closed by W4b-1 or W4b-3 | SPEC §7.2 intro + §7.2.1/§7.2.3: "W4b-1 (the parity foundation) and W4b-3 (the variable-width breadth) carry no row gate." `G-W4b-1-CODEC-HARNESS` is compile+parity; `G-W4b-3-CODEC-BINDINGS` is compile+parity. Only `G-W4b-2-CODEC` carries the conditional-admission rule. CH5-relevant: if a non-row-moving sub-wave carried a row gate, the cohort could close the codec on a sub-wave that ships no JSON consumer — a paper-close that the pairing exists to prevent. By pinning the row gate to W4b-2 alone (the sub-wave with the JSON production consumer, paired with W4a), the V3 fold keeps the codec's only non-vacuous exit gate at the one sub-wave that actually moves rows. The split is coupling-honest. | ACCEPT |
| V25 | W4c EOR3 — `FEAT_SHA3`-conditional, scalar fallback unconditional (no default-rewire) | SPEC §7.3 owner table: "The scalar shift-XOR ladder stays the unconditional fallback." Exit gate clause 4: "a capability-conditional specialisation, the same admissibility shape as `digit_mac` (DotProd-gated)." Pre-blocked routes carry the REDRESS 88 differential and the HANDOFF §5 "PMULL prefix-XOR as default hot paths" disposition. The EOR3 path is wave-internal and host-gated — no default-hot-path rewire, no Pass-Omega dependency (P3-B §4). | ACCEPT |
| V26 | W4d CTZ — host-capability-gated at a non-default call site, `rbit+clz` fallback | SPEC §7.4 owner table: "The `rbit + clz` form stays the unconditional fallback." Exit gate clause 3: "The CTZ body is host-capability-gated at the non-default call site." Pre-blocked routes carry the REDRESS 89 differential and the W10b six-row maintain gate as "the binding hard blocking precondition." No default rewire. | ACCEPT |
| V27 | `substrate_cardinality` schema field gate-consumed at W3; no 37th column | P3-D §2.2 schema field 33 `substrate_cardinality` = `one`; §2.3 W3 row "`substrate_cardinality` MUST stay `one`." SPEC §0.y lists it in the 36-field set; §6 exit gate clause 8 binds it. P3-D `§0 V3 fold footer` item (2) records the §2 schema confusion resolved to the 36-identifier set; the V3 SPEC §0.y carries it verbatim, "No SK-V9 wave adds a 37th." No `union_class_column_status`-style 37th-column invention — the substrate identity rides the existing `substrate_surface` / `structural_projection_status` / `substrate_cardinality` fields. The V2-CH5-6 resolution holds in V3. | ACCEPT |
| V28 | C8 checkasm-parity backfill — bundled precondition, distributed across the W4b sub-waves | P3-A §3: C8 "is not a wave. Its four test files distribute as same-wave preconditions of C4 and C5." SPEC §7.1 names `checkasm_string_block.rs` landing FIRST and blocking W4a; §7.2.1 names `checkasm_escape_codec.rs` as the W4b-1 deliverable that "is the gate every W4b-2/W4b-3 body must clear"; §7.3 extends `checkasm_bitmap_prefix_xor_64.rs`; W4d's CTZ correctness rides W4a's `checkasm_string_block.rs` (§7.4 "no separate checkasm file"). The W4b three-way split placed the codec checkasm harness at W4b-1 as the chain's admission precondition — every codec body W4b-2/W4b-3 lands is diffed against a same-bracket harness. No orphan-kernel-untested route. | ACCEPT |
| V29 | Dispatch-draft per-sub-wave triumvirate — each codec sub-wave a fresh triumvirate, own 75-min redress | Dispatch §"Per-Wave Triumvirate Protocol" Phase 3: "Each W4 sub-wave — including each of W4b-1/W4b-2/W4b-3 — gets its own 75-min redress; that is the point of the sub-wave structure." SPEC §2.2: "W4b is therefore cut along the P2-E §7.4 slice seams into three sub-waves, each inside its own 75-min redress." The W4b split resolves the CH4 ~1,045-net-LOC codec-cap defect without violating the cascade — each codec sub-wave's kernel-into-union wiring is same-commit (W4b-2 specifically; W4b-1/W4b-3 ship reference/breadth, not a hot-path kernel needing the union). The triumvirate structure is coupling-clean. | ACCEPT |
| V30 | dispatch-draft `§0 V3 fold footer` + Required-Reading map coherent with the W4b split | The dispatch §0 V3 fold footer records the rebind to "the W4b three-way sub-division"; the Required-Reading map item 11 splits "W4b-1 / W4b-2 / W4b-3 → P2-E §7.1 (W4b-1 = S1/S6, W4b-2 = S2/S3/S5/S7/S8/S11, W4b-3 = S4/S9/S10)"; the Wave Manifest, the cascade-lock paragraph, the Falsifiability-Gate facts, and the Convergence section ("W0 + W1-W3 + the six W4 sub-waves + W5 = 11 brackets, inside the ≤12 skinny-bracket ceiling") all rebind consistently. The dispatch contract an implementation agent follows ships the same-commit kernel-into-union wiring per sub-wave; no coupling fault is propagated to execution. | ACCEPT |

Spot-check count: 30 dispositioned rows — exceeds the ≥20 task minimum.

---

## §3 — Aggregate verdict

Dispositions: 30 rows — **29 ACCEPT, 1 REVISE, 0 REJECT**.

ACCEPT rate 29/30 = **96.7%** — clears the §3Z ≥95% convergence bar.

The V3 comprehensive integration fold is decisive on coupling. The
single root cause CH5 V1 named — the cascade split conflated across
three "same-wave" relations — was closed at V2 by the SPEC §2.2
disambiguation; the V3 fold carried §2.2 verbatim and extended it
cleanly across the W4b three-way sub-division. The V3 fold's structural
change — sub-dividing W4b into W4b-1/W4b-2/W4b-3 to resolve the CH4
codec-cap defect — is verified coupling-clean:

- **The cascade-lock disambiguation survives all seven V3 artefacts
  (V1, V11).** No artefact describes a kernel landing without the W3
  union; W3 ≺ W4 holds across SPEC, dispatch, P3-A/B/C/E.
- **Every W4 sub-wave wires its kernel into the already-landed W3 union
  same-commit (V2-V8).** W4a, W4b-1, W4b-2, W4b-3, W4c, W4d each carry
  same-commit kernel-into-union (or, for W4b-1/W4b-3, reference/codegen)
  wiring; no orphan ships. The double-deep W4d dependency (W3 + W4a) is
  placed and gated.
- **The W4b three-way split manufactured no orphan and dropped no
  pre-block (V4, V6, V13, V19, V24).** W4b-1 ships no kernel (a
  reference + a harness); W4b-3's variable-width body is codegen-template
  consumed; the row gate is pinned to W4b-2 alone — the pairing cannot
  be paper-closed by a non-row-moving sub-wave.
- **W3 keeps substrate cardinality at one (V9).** The decision *not* to
  sub-wave W3 (V10) is the coupling-correct one — a W3a/W3b split would
  orphan the class column from its SIMD producer. The ≤110-min
  CHALLENGE-gated extension is a timing instrument, not a substrate
  change.
- **No V3 wave introduces a sidecar, a parallel substrate, or a
  Track-1≡Track-2 dishonesty (V15, V16).** The schema stays at 36
  fields with substrate cardinality one; no 37th-column invention (V27).
- **Both V2 residuals are addressed (V12, V14).** V14 — the stale
  "Each row is one wave" phrase — is folded: §2 is W1/W2/W3, §2a is the
  six W4 sub-waves. V12 — the codec re-bodies a parser-owned
  materialiser — is re-confirmed coupling-clean on the REDRESS-82
  differential and carried as a §4 watch-item; it remains a REVISE-grade
  framing-honesty flag, not a coupling fault.

The one REVISE (V12) is honesty-of-framing on an architecturally-sound
route — the codec consumer is parser-owned, but the route re-bodies a
pre-existing production path and ships no new substrate or scratch
buffer. It does not re-open Lock 1 (`LOCKS.md:34`) and it is not a
REDRESS sidecar route. At 96.7% the cohort clears the formal §3Z bar.

Verdict: **ACCEPT.** The V3 comprehensive fold closed the V2
coupling residuals, unified the manifest across all seven artefacts,
and the W4b three-way sub-division carries the cascade-lock, the
W4a+W4b-2 pairing, and substrate cardinality one intact. CH5 clears
for V3 at 96.7%.

---

## §4 — Remaining coupling risks

No structural coupling fault remains. The following is a single
implementation-time watch-item, not a P3-artefact defect.

**N1 (REVISE, V12) — the W4b-2 codec consumer is a parser-owned string
materialiser; the differential is honesty-of-framing.** W4b-2 re-bodies
the codec onto the already-wired `unescape_four_unicode_escapes` x4 path
in `parse-that-regex/src/lib.rs` plus the `runtime/src/grammars/json/sink.rs`
call-site swap — both parser-owned. This is architecturally sound (the
path already exists, is already a production consumer, and W4b-2 ships
no new substrate and no new scratch buffer) and CH5 does not reject it.
But the cohort must keep the REDRESS-82 five-axis material differential
(SPEC §7.2.2 "Pre-blocked routes", P3-E §3.4) load-bearing through
implementation: if the W4b-2 redress agent ever re-bodies the codec
onto a *new* parser-owned site, or adds a retained per-`\u`
validator/scratch, the differential collapses and the route becomes the
REDRESS-82 parser-owned-scratch shape. The W4b-2 plan-time CHALLENGE
must re-confirm the consumer is the pre-existing x4 path + the `sink.rs`
call-site swap only — no new parser-owned helper. The W4b three-way
split sharpens this audit by isolating the row-moving consumer at W4b-2
(W4b-1 ships a reference, W4b-3 ships codegen — neither touches the JSON
hot path). This is a watch-item for the W4b-2 triumvirate, not a
P3-artefact defect.

**No structural coupling risk remains.** The P2-A union architecture
stands unchanged since V1. The V1-demanded wave-sequencing correction
was delivered at V2 and survives V3: the cascade-lock is disambiguated
in the SPEC, every W4 sub-wave wires its kernel into the already-landed
W3 union same-commit, the union-substrate codec consumer is
dispositioned (collapsed to the already-wired x4 path with the
REDRESS-82 differential carried), the three "same-wave" relations are
named distinctly, and the schema stays at 36 fields with substrate
cardinality one. The V3 fold's W4b three-way split — its one structural
change — is coupling-clean: it manufactured no orphan, dropped no
pre-block, and kept the W4a+W4b-2 pairing exact. The single §4 item is
a surgical implementation-time watch-item; it does not re-open Lock 1
or any REDRESS sidecar route.
