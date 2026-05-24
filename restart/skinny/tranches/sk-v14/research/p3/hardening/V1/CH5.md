# SK-V14 S-P3 CHALLENGE V1 — CH5 HIDDEN COUPLING

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-23.
Lens: CH5 HIDDEN COUPLING (per PASS-3-SYNTHESIS-PLAN.md §3 + CHALLENGE-CONTEXT.md:32).
Scope: adversarial review of S-P3 V1 cohort (6 P3 artefacts at HEAD `8f4756113` + `sk-v14/SPEC.md` + `sk-v14/DISPATCH-PROMPT.md`) for parallel-substrate / sidecar-producer / renamed-scanner (Lock 1) / Track 1 ≡ Track 2 dishonesty / parser-owned structural projection / retained cursor / aux density table / sidecar event vector / substrate-union breach across every wave W0..W11.
Hard cap: 30 min. WRITE-ONLY (no git operations); aggregator commits 8 hardening files atomically.

## §0 — Lens binding + verification mandate

Per `PASS-3-SYNTHESIS-PLAN.md:134-138`:

> **CH5 HIDDEN COUPLING** — does any wave introduce a parallel substrate, a sidecar producer, a renamed scanner (Lock 1), or a Track 1 ≡ Track 2 dishonesty? Does the SPEC's exit-gate language forbid a parser-owned structural projection / retained cursor / aux density table / sidecar event vector? The substrate union must hold across every wave.

Per `CHALLENGE-CONTEXT.md:32` (V1 disposition focus verbatim):

> No wave introduces parallel substrate / sidecar producer / renamed scanner (Lock 1) / Track 1 ≡ Track 2 dishonesty. SPEC exit-gate language forbids parser-owned structural projection / retained cursor / aux density table / sidecar event vector. Substrate union holds across every wave. §2.Y NF-CH6-4 canonical-name binding for long-string-body SIMD scan (P2-A C2 / P2-E Gap 1 / P2-F C1+C2 → P3-A C1) consolidated to ONE primitive.

Per LAC-1E-12 procedural addendum + S-P2 V3 dispatch-context §2 + memory `[redispatch-empty-return]`: every cited path:line was re-executed at the cycle HEAD (`8f4756113`) before this lens authored a disposition. Executable verification commands appear inline per disposition row.

## §1 — Audit-substrate per check (the 6 CH5 sub-axes)

### §1.1 — Parallel substrate introduction (Lock 1 primary axis)

The substrate-union ceiling at `restart/locks/LOCKS.md:48` reads verbatim:

> **Tape is the substrate, properly unioned with direct-to-struct; columnar SoA is dead; orthogonal codepaths and parallel substrates are dead.** ... **2026-05-21 v+1 substrate-ceiling fold**: Skinny Track 2 remains a substrate-ceiling probe, not a second substrate. ... Track 2 measures whether the same `runtime::tape` + `bbnf-simd` APIs can reach the SOTA envelope when hand-coded against the APIs codegen will emit; it does not authorize hidden runtime identity, parser-owned sidecars, or a parallel representation.

The audit substrate per shortlist candidate (P3-A §2 + §2.1; verified at cycle HEAD via `grep -n "Substrate target" restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md`):

| Candidate | `substrate_target` declared (per Lock 1 v+1 allowed values) | Parallel-substrate risk |
|---|---|---|
| C1 `long_string_body_simd_scan` | `local_temp_only` (SIMD masks transient; emit folds into existing `compact_mask`) | NONE — SIMD producer is transient, not retained; `compact_mask` already in singular tape pipeline |
| C2 `structural_index_singular_substrate_consumer` | `existing_tape` (retention_lifetime `output_row`; policy_owner `generated_grammar`) | NONE — explicit "OPPOSITE of REDRESS 96/97/98: removes parallel-substrate consumers by re-routing existing consumers to existing producer" (p3a:83) |
| C3 `digit_block_simd_accumulate` | `direct_sink` (f64/i64 emit) + `local_temp_only` (per-chunk digit accumulator) | NONE — emit is into direct sink; chunk accumulator dies at function scope |
| C4 `unicode_escape_neon_nibble_decode` | `local_temp_only` (decoded codepoint buffer) | NONE — buffer is unwrap-then-emit |
| C5 `parse_attribution_envelope_cracker` | N/A (build invariant — process discipline; no kernel, no substrate) | NONE — `cfg_attr(feature = "parse-attribution", inline(never))` flip; ZERO substrate delta |
| C6 `force_inline_lto_envelope_discipline` | N/A (build invariant — LTO + force-inline, NOT a substrate) | NONE — codegen template + LTO; ZERO substrate delta |
| C7 `ascii_whitespace_skip_64` | `local_temp_only` (returns offset; NO positions emitted) | NONE — pure skip primitive; no retained state |
| C8 `BackendShape::SinkOnly` activation | `direct_sink` (the activation REMOVES a retained-substrate path — ELIDES `TapeBuilder` construction) | NONE — explicit "OPPOSITE of REDRESS 96/97/98 family; closes a retention path, does not open one" (p3a:161) |

**8/8 candidates carry an explicit `substrate_target` per Lock 1 v+1 declaration triple at `LOCKS.md:73-82`.** Two of eight (C2, C8) are explicitly retention-*subtractive* (re-route existing consumer to existing producer; elide retained TapeBuilder construction). Six of eight (C1, C3, C4, C5, C6, C7) introduce NO retained substrate. Substrate union holds across the shortlist. **VERDICT: ACCEPT (zero parallel substrate introduced; substrate-union honesty preserved).**

### §1.2 — Sidecar producer / sidecar event vector / aux density table (the SK-V6 + SK-V9 W3 ceilings)

Per `SPEC.md:211` non-negotiable verbatim:

> No parser-owned structural cursor / facts / aux table / density cache / sidecar event vector.

Per `SPEC.md:212` non-negotiable verbatim:

> No parallel or sidecar substrate.

Per `SPEC.md:1083` global block verbatim:

> New directive, BIR variant, substrate surface, `BackendShape`, `UnionTape`, public substrate API, parser-owned cursor/facts, sidecar substrate, and parallel substrate.

Per `SPEC.md:1102` REDRESS watch-list verbatim:

> REDRESS 96-98: full class-column vectors, streaming structural cursors, class-lane-only replays, parser-owned sidecars, UnionTape-style retained structures per Lock 1 v+1 substrate-ceiling history.

Cross-checked against P3-E supplementary §3 cohort census (`grep -n "Item 96\|Item 97\|Item 98\|union-substrate intrinsic" restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`):

> **SK-V9 W3 union-substrate intrinsic block:** Item 96 ... full class-column vectors REJECTED ... Item 97 streaming-cursor (no allocation) REJECTED ... Item 98 class-lane-only ... "**No SK-V14 wave may force, amend, or split W3 to preserve the same union-substrate hypothesis without first ...**"

P3-E §3.1 names these as PERMANENT pre-blocks across W0-W11. P3-C §4 line 506 + P3-C §3.7 + SPEC §15 + SPEC §1 lines 211/212 all carry the same surface verbatim.

**VERDICT: ACCEPT — sidecar producer / aux density table / sidecar event vector / class-lane-only / streaming structural cursor are all explicitly forbidden at SPEC §1 non-negotiables (lines 211-212) + SPEC §15 REDRESS watch-list (line 1102) + P3-E PERMANENT pre-blocks. Exit-gate language verbatim mirrors `LOCKS.md:84-90` v+1 substrate-ceiling.**

### §1.3 — Renamed scanner (Lock 1 no-rename clause)

Per `LOCKS.md:48` (Lock 1 2026-05-04 reframe verbatim):

> Lock 1's spirit (no parallel substrate; no orthogonal codepath; no Vec<OpenFrame>::clone pathology) holds; the no-rename clause is amended.

The **no-rename clause was AMENDED** at the 2026-05-04 reframe — Lock 1's binding is now spirit-based (no parallel substrate; no orthogonal codepath), NOT name-based. The CHALLENGE-CONTEXT.md:32 phrasing "renamed scanner (Lock 1)" maps to: does any wave rename an existing scanner in a way that obscures parallel-substrate or orthogonal-codepath introduction?

Audit per shortlist (verified at cycle HEAD via `grep -n "scan_string_special_block\|scan_structurals\|ascii_whitespace_skip\|byte_class_from" restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md | head -20`):

- **C1 `long_string_body_simd_scan`** owner path `crates/bbnf-simd/src/aarch64/string_block.rs` (p3a:64-65): "existing `scan_string_special_block` at `:57` extends with a 64-byte sweep wrapper" — this is a **wrapper extension**, not a rename. The existing 16-byte body remains; the new 64-byte sweep is `scan_string_special_block_sweep_64` (P2-E Gap 1 canonical name); composes existing `escape_mask_64` + `classify_tbl4` + `bitmap_prefix_xor_64_scalar`. NO rename; SIMD producers compose; substrate is `local_temp_only`. VERIFIED.
- **C2 `structural_index_singular_substrate_consumer`** owner path (p3a:77): `crates/bbnf-bench/src/generated_real_typed.rs:2949-3003` rewrite of `DirectParser::skip_value` body; "single substrate; no new producer" — the rewrite consumes the *existing* `scan_structurals` substrate at `crates/runtime/src/grammars/json/scan.rs:22`. NO new scanner; NO rename. VERIFIED.
- **C7 `ascii_whitespace_skip_64`** owner path (p3a:142): "new sibling fn `ascii_whitespace_skip_64` to existing `scan_string_special_block` at `:57`" — a **new function**, not a rename of an existing one; scalar reference `byte_class_from_eq_set_64_scalar` at HEAD already exists. NO rename. VERIFIED.

No shortlist candidate renames an existing scanner. The C1 NF-CH6-4 binding (P2-A C2 ∪ P2-E Gap 1 ∪ P2-F C1+C2) consolidates THREE convergent identifiers under ONE canonical primitive name `long_string_body_simd_scan` + ONE canonical scalar-ref function — this is the OPPOSITE of a rename; it is a name-collapse from three to one. **VERDICT: ACCEPT (no-rename clause honoured in spirit; canonical-name binding enforced per S-P2 V3 §6.2).**

### §1.4 — Track 1 ≡ Track 2 dishonesty (P-7 pattern pre-block + CH5 gate column)

The Track 1 / Track 2 anti-coupling gate is the primary novel CH5 instrument in SK-V14. Per `SPEC.md:135` Section 0.4 column 6 verbatim:

> `track2_entry_point` (NEW — CH5; symbol path of Track 2 oracle; gate-json rejects rows where Track 1 and Track 2 entry-point symbols share a common ancestor in runtime::tape:: beyond public Tape / OffsetFlags types)

Per `SPEC.md:1075` Section 15 P-7 pattern pre-block verbatim:

> **P-7** — Track 1 ≡ Track 2 dishonesty. SK-V14 bench harness must keep Track 1 (generated) structurally distinct from Track 2 (independent oracle); any plane collapse fails gate. The `track2_entry_point` column (per §0.4) is the gate enforcement.

Per `SPEC.md:930` W9 Section 12 exit gate verbatim:

> Track 2 does not call generated SinkOnly, generated typed helpers, generated Track 1, or a shared benchmark-private parser; `track2_entry_point` column verifies symbol-path divergence.

Per `SPEC.md:991` W10 Section 13 exit gate verbatim:

> Track 1 / Track 2 structural independence proven; `track2_entry_point` populated.

Per `P3-D §2.2` (`p3d-telemetry-schema.md:83-87`) verbatim:

> **`track2_entry_point`** — NEW (CH5 hidden-coupling). SEMANTICS: Symbol path of the Track 2 oracle entry-point (e.g. `bbnf_runtime::json::typed::parse_typed_entry`). Distinct from the Track 1 generated entry point. POPULATION SOURCE: `bbnf-bench` harness ... REJECTION RULE: Empty → reject. Track 1 and Track 2 entry-point symbol paths share a common ancestor in `runtime::tape::` beyond the public `Tape` / `OffsetFlags` types → reject per SYNTHESIS §2 line 240 (CH5 hidden-coupling pre-block — Track 1 ≡ Track 2 dishonesty).

Per P3-D §3.5 verbatim:

> **Hidden-coupling gate (CH5).** For every row, the gate parses `track2_entry_point` and computes the common-ancestor prefix vs Track 1 entry-point. Common ancestor in `runtime::tape::` beyond `Tape` / `OffsetFlags` → reject. Test: artificially point Track 2 at a private tape internal and confirm rejection.

Per P3-C §3.7.2 line 331 + §3.8.2 line 368 + §3.10 line 437 — every admit wave (W8 + W9 + W10) carries the symbol-path divergence gate explicitly.

Per P3-C §4 line 510 verbatim:

> No Track 1 / Track 2 coupling or benchmark-private parsers (SYNTHESIS §0.4 P-7).

The CH5 hidden-coupling gate `track2_entry_point` is wired:
- W0 W1 SPEC §3-§4 → telemetry column emission + gate-json rejection rule installation (verified at SPEC.md:135 + SPEC.md:344);
- W7 SPEC §10 (PRUNE-5) → CSP-emitted `BackendShape` dispatch path itself MUST NOT route via grammar-name (per SPEC.md:810);
- W8 SPEC §11 + W9 SPEC §12 + W10 SPEC §13 → every admit row gates on `track2_entry_point` symbol-path divergence vs Track 1 (verified at SPEC.md:930 + SPEC.md:991 + SPEC.md:1075).
- P3-D §3.5 names the artificial-pointing test: "artificially point Track 2 at a private tape internal and confirm rejection" — executable verification mandate met.

**VERDICT: ACCEPT (CH5 anti-coupling instrument is the most strongly wired novel SK-V14 telemetry column; every admit wave gates on it; rejection-rule semantics are explicit and testable).**

### §1.5 — Parser-owned structural projection / retained cursor (the SK-V5 + SK-V6 + SK-V9 W3 ceilings)

Per `SPEC.md:211` non-negotiable verbatim:

> No parser-owned structural cursor / facts / aux table / density cache / sidecar event vector.

Per `SPEC.md:1096-1097` REDRESS watch-list verbatim:

> REDRESS 49-55: no-allocation visitor, parse-time aux side tables, EventCursor, parser-local structural-mask cursor, decoded stats sink, quote-source fused string materializer.
> REDRESS 59-65, 72/83: retained string-boundary collapse, always-wide or delayed-wide scanning, Unicode validator/classifier retries, object/key carry, global/direct/Track 2 cap-16, generated-retained StringBlock16 tiny probe.

Cross-checked at P3-E supplementary §3 (verified at HEAD via `grep -n "EventCursor\|structural-mask cursor\|aux side" restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`):

- Item 51-55 (SK-V5 retained structural-mask cursor + parse-time aux side tables) PERMANENT pre-block;
- Item 96/97/98 (SK-V9 W3 union-substrate intrinsic block) PERMANENT pre-block "**No SK-V14 wave may force, amend, or split W3 to preserve the same union-substrate hypothesis...**".

C2 substrate-union-typed-skip explicitly re-routes the existing `DirectParser::skip_value` body to consume the *existing* `StructuralIndex` substrate at `crates/runtime/src/grammars/json/scan.rs:22` (p3a:77-78); per p3a:84 explicitly: "OPPOSITE of REDRESS 96/97/98 — removes parallel-substrate consumers by re-routing existing consumers to existing producer". **NO new retained data; NO new class column; NO new cursor.** VERIFIED at HEAD via `grep -n "fn scan_structurals\|fn skip_value" skinny/crates/runtime/src/grammars/json/scan.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs`.

C8 SinkOnly activation explicitly ELIDES the existing `TapeBuilder` construction in the direct-plane `parse_value_direct` envelope (p3a:155-161); per p3a:161 explicitly: "**OPPOSITE of REDRESS 96/97/98 family** — closes a retention path, does not open one". **VERIFIED.**

**VERDICT: ACCEPT — exit-gate language verbatim forbids parser-owned structural projection / retained cursor / aux density table / sidecar event vector across every wave; both substrate-shape candidates (C2, C8) explicitly remove or re-route retention rather than introduce it.**

### §1.6 — §2.Y NF-CH6-4 canonical-name binding (the consolidation requirement)

Per CHALLENGE-CONTEXT.md:32 verbatim:

> §2.Y NF-CH6-4 canonical-name binding for long-string-body SIMD scan (P2-A C2 / P2-E Gap 1 / P2-F C1+C2 → P3-A C1) consolidated to ONE primitive.

Per `SPEC.md:222` non-negotiable verbatim:

> Any wave admitting the long-string-body SIMD scan primitive (the three convergent identifiers `long_string_body_simd_scan` / `scan_string_special_block_sweep_64` / quote-aware classifier composition per S-P2 V3 §6.2) MUST commit to ONE canonical primitive name + ONE canonical scalar-ref function — three orthogonal SIMD bodies for one primitive is REJECT per Lock 14 v+1.

Per `SPEC.md:1110` Section 15 S-P2 carry-forward pre-block verbatim:

> **Three orthogonal SIMD bodies for one primitive** per S-P2 V3 §6.2 / P2-F §2.Y. The three convergent identifiers `long_string_body_simd_scan` / `scan_string_special_block_sweep_64` / quote-aware classifier composition admit under ONE canonical primitive name + ONE canonical scalar-ref function at admission time.

Per `p3a-candidate-shortlist.md:64` (Candidate C1 verbatim):

> **P2 candidate IDs (consolidated per §6.2):** P2-A C2 `long_string_body_simd_scan` (`p2a:184` row) ∪ P2-E Gap 1 `scan_string_special_block_sweep_64` (`p2e:97-110`) ∪ P2-F C1 structural-byte SIMD classify + C2 quoted-string boundary scan (`p2f:68-88`). One canonical primitive across three convergent identifiers per the §2.Y cross-axis tracking note at `p2f:231-239`.

Per `p3a:184` §2.1 census verbatim:

> **§2.Y NF-CH6-4 canonical-name binding compliance:** C1 is the ONE canonical primitive name across P2-A C2 + P2-E Gap 1 + P2-F C1+C2 per HARDENING-S-P2-V3-CONSOLIDATED §6.2 verbatim binding. Zero candidates admit three orthogonal SIMD bodies for one primitive.

Per `p3b-wave-sequencing.md:32` verbatim (P3-B carry-forward):

> **§2.Y NF-CH6-4 canonical-name binding** — ONE canonical primitive name + ONE canonical scalar-ref function across the three convergent identifiers (P2-A C2 `long_string_body_simd_scan` / P2-E Gap 1 `scan_string_special_block_sweep_64` / P2-F C1+C2 quote-aware classifier composition), all grounded on the `unescape_string` direct rank-1 46.7 % `unicode_escapes` hot-leaf (P1-E §2.2). Binds R6/R7/R8 admission, never three orthogonal SIMD bodies.

Per `p3b:354` §4 cross-wave pre-block verbatim:

> **No three orthogonal SIMD bodies for the long-string-body SIMD scan primitive** per S-P2 §6.2 §2.Y canonical-name binding. ... A wave admitting any of the three under three names fails CH2/CH7.

**Five-witness convergence on the canonical-name binding** (SPEC §1 line 222 + SPEC §15 line 1110 + P3-A §2.1 line 184 + P3-A C1 §2 line 64 + P3-B §1.1 line 32 + P3-B §4 line 354).

The C1 entry at P3-A §2 also specifies the canonical owner path (`crates/bbnf-simd/src/aarch64/string_block.rs`) + canonical scalar-ref function (`scan_string_special_block_scalar` at `:31`) + canonical SIMD body wrapper extension (sweep_64 to existing `scan_string_special_block` at `:57`). **VERIFIED at HEAD via `grep -n "fn scan_string_special_block" skinny/crates/bbnf-simd/src/aarch64/string_block.rs`.**

**VERDICT: ACCEPT — canonical-name binding consolidated to ONE primitive across five witnesses; zero risk of three-orthogonal-SIMD-bodies admission.**

## §2 — Per-wave disposition (W0..W11; SPEC §3-§14)

For each of 12 SPEC waves, the hidden-coupling axes (parallel substrate / sidecar producer / Lock 1 spirit / Track 1≡Track 2 / parser-owned projection / retained cursor / aux density / sidecar event vector) are gated.

### §2.1 — W0 (SPEC §3 — Baseline Profile + Telemetry Lock)

Owner-paths: `bbnf-bench/`, `xtask/src/`, `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, `research/wave-0-*.md` (SPEC.md:317-323). Behavior-LOC = 0 (SPEC.md:237). The wave INSTALLS the CH5 instruments (`track2_entry_point` + Lock 1 triad columns per SPEC §0.4 line 135 + 153). Same-wave consumer is `xtask gate-json` (SPEC.md:363).

- Parallel substrate: NONE (telemetry-only).
- Sidecar producer: NONE (gate-consumed columns only).
- Renamed scanner: NONE.
- Track 1 ≡ Track 2 dishonesty: ACTIVELY PRE-BLOCKED — W0 installs the `track2_entry_point` column + rejection rule per SPEC.md:135 + P3-D §2.2 line 87.
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE.

**DISPOSITION: ACCEPT.**

### §2.2 — W1 (SPEC §4 — Comparator Rebind + Per-Iter Equality + PRUNE-1)

Owner-paths: `bbnf-bench/benches/json_parity.rs`, `bbnf-bench/src/real_typed_struct.rs`, `xtask/src/main.rs` + `xtask/src/gate.rs`, `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, `REDRESS.md` (SPEC.md:381-389). Behavior-LOC = 0 production parser-change; 600-1.08k comparator-binding + 250 PRUNE-1 revert.

- Parallel substrate: NONE (harness-only).
- Sidecar producer: NONE.
- Renamed scanner: NONE.
- Track 1 ≡ Track 2 dishonesty: ACTIVELY PRE-BLOCKED — W1 wires R1+R2 comparators; `per_iter_equality` PASS-per-iter inside timing region (SPEC.md:415-417); P-2 (sonic_rs::from_slice mislabelled as strict) explicitly blocked (SPEC.md:441).
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE.

**DISPOSITION: ACCEPT.**

### §2.3 — W2 (SPEC §5 — regen-css xtask R4)

Owner-paths: `xtask/src/main.rs`, `xtask/src/regen_css.rs` (new), `runtime/src/grammars/css_l4_*/` (generated output destination), `crates/core/src/runtime/css_l4/` (generated output destination dual-tree). Behavior-LOC = 2.0k xtask scaffolding; generated output uncounted.

- Parallel substrate: NONE (xtask emission only; consumes existing `bbnf-codegen` template machinery).
- Sidecar producer: NONE.
- Renamed scanner: NONE (xtask binary parametrised per SPEC.md:481).
- Track 1 ≡ Track 2: N/A (xtask wave; no admit).
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE.

**DISPOSITION: ACCEPT.**

### §2.4 — W3 (SPEC §6 — Production CSS Corpora R5)

Owner-paths: `corpora/css-l4-sk-v14/`, `corpora/css-l4-sk-v14/manifest.md`, `bbnf-bench/src/css_l4_corpus.rs`. Behavior-LOC = 200 corpora-loader; corpora bytes uncounted.

- Parallel substrate: NONE (corpora bytes only).
- Sidecar producer: NONE.
- Renamed scanner: NONE.
- Track 1 ≡ Track 2: N/A (corpora wave; no admit).
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE.

**DISPOSITION: ACCEPT.**

### §2.5 — W4 (SPEC §7 — PRUNE-2 delete 7 CSS templates + revert 24 CSS admits)

Owner-paths: `codegen/src/css_l4_*_templates/` (DELETE), `codegen/src/css_l4_*_provider.rs` (DELETE), `runtime/src/grammars/css_l4_*/` (DELETE), `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, `REDRESS.md`. Behavior-LOC = 500 revert (negative net).

- Parallel substrate: NONE (deletion + revert).
- Sidecar producer: NONE.
- Renamed scanner: NONE.
- Track 1 ≡ Track 2: N/A.
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE — POSITIVELY DISCHARGES the hand-written CSS template fake-`@generated` cluster (SPEC.md:597 detector empty post-PRUNE).

**DISPOSITION: ACCEPT.**

### §2.6 — W5 (SPEC §8 — PRUNE-3 Lock-14 refactor: trait dispatch + grammar-agnostic generator)

Owner-paths: `passes/src/lib.rs` (replace `RuntimeProvider` enum), `codegen/src/lib.rs:167-209` (replace 8 per-grammar match arms), `codegen/src/grammar_provider.rs` (new generic generator template), `bbnf-bench/src/lock14_baseline.rs` (extend baseline gate forward-invariant). Behavior-LOC = 1.4k C-1 part-A.

- Parallel substrate: NONE (trait-dispatch refactor; no substrate change).
- Sidecar producer: NONE.
- Renamed scanner: NONE.
- Track 1 ≡ Track 2: N/A (structural refactor; no admit).
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE.

**DISPOSITION: ACCEPT.**

### §2.7 — W6 (SPEC §9 — PRUNE-4 9 sub-waves: per-grammar runtime collapse)

Owner-paths: `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/` (9 per-grammar directories; collapse onto template-emitted output); `crates/core/src/runtime/{<G>/parse_with.rs}` LegacyPath shim removals (9 instances); `crates/core/src/runtime/builder_template.rs` + `arena_template.rs` Pattern H opt-out rewrite. Behavior-LOC = 2.0k C-1 part-B aggregate; generated uncounted.

- Parallel substrate: NONE (the LegacyPath shim removal is positively retired; the Pattern H opt-out enshrinement rewrite removes a *documentary* parallel-codepath risk).
- Sidecar producer: NONE.
- Renamed scanner: NONE — per-grammar runtime files become emitted output from the W5 grammar-agnostic generator template.
- Track 1 ≡ Track 2: N/A.
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE.

**DISPOSITION: ACCEPT.**

### §2.8 — W7 (SPEC §10 — PRUNE-5 wire W8 + W9 from SCAFFOLD to LOAD-BEARING) ← **CH5 HIGHEST-RISK WAVE**

Owner-paths (SPEC.md:775-782): `passes/src/` (wire `per_grammar_policy` + `same_substrate_union` into compile + lower + runtime paths beyond gate-layer-only), `codegen/src/` (consume CSP-selected shape), `runtime/src/` (runtime honors CSP-selected shape), `bbnf-bench/src/{bin/gate.rs, lock14_baseline.rs, report.rs}` (extend gate enforcement).

The W7 wave introduces the name **`same_substrate_union`** into compile + lower + runtime paths (SPEC.md:799). Per CHALLENGE-CONTEXT.md:32 hidden-coupling focus: **the term "union" itself is the SK-V9 W3 audit-falsified frame** (P3-E §3 verbatim "No SK-V14 wave may force, amend, or split W3 to preserve the same union-substrate hypothesis"; LOCKS.md:84-90 verbatim "REDRESS 96/97/98 are binding substrate-ceiling history").

**Per SPEC.md:822 pre-blocked routes for W7 verbatim:** "(UnionTape-style retained structures per Lock 1 v+1)". Per SPEC.md:802 task 5 verbatim: "The shape consumer in `skinny/crates/codegen/src/lib.rs` MUST dispatch on the CSP-emitted `BackendShape` enum alone — no `match grammar { Json => ..., CssL4 => ... }` arm may appear in the dispatch path per SYNTHESIS §4." Per SPEC.md:810 exit-gate verbatim: "The shape consumer dispatches on `BackendShape` alone — Lock 14 grep `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>' skinny/crates/codegen/src/lib.rs` returns ZERO matches."

The `same_substrate_union` name *enforces* substrate-union honesty under Lock 1 v+1 (per-shape `substrate_target` triad declared per SPEC.md:793; CSP-emitted `BackendShape` dispatch per SPEC.md:810) — it does **NOT** construct a UnionTape or a parallel substrate. The exit-gate verbatim forbids UnionTape-style retained structures (SPEC.md:822).

However: **the SK-V9 W3 audit-falsification frame's *name*** is "same-substrate union hypothesis" (P3-E §3 verbatim Item 96 line 196-210 + LOCKS.md:84-90). The W7 *module name* `same_substrate_union` is the *enforcement* module per ORCHESTRATOR-PROMPT.md:124-126, not the audit-falsified data structure. **But this naming proximity is a CH5 hidden-coupling tension** — a reader unfamiliar with the SK-V9 W3 history could conflate the W7 enforcement module with the SK-V9 W3 retired union-substrate. The SPEC's W7 plan does not yet author a clarifying gloss distinguishing the two.

**Minor REVISE 1:** SPEC §10 (W7 tasks) should add an inline clarifying gloss at the `same_substrate_union` mention (SPEC.md:799): "`same_substrate_union` is the W9 SCAFFOLD enforcement module per ORCHESTRATOR-PROMPT.md:124-126 that ENFORCES Lock 1 substrate-union honesty (singular runtime::tape consumer per shape; CSP-emitted BackendShape dispatch); it is NOT a UnionTape, NOT a class-column union, NOT a parallel substrate; SK-V9 W3 retired union-substrate hypothesis (REDRESS 96/97/98) is permanently pre-blocked per LOCKS.md:84-90 + SPEC §15 line 1102."

Additionally — **the W7 pre-blocked-routes section (SPEC.md:817-822) cites REDRESS 96-98 only as a parenthetical at line 822**. Per P3-E §3 + P3-C §4 line 506, REDRESS 96/97/98 are PERMANENT pre-blocks across W0-W11; the W7 SPEC should escalate these to a top-level pre-block bullet (not parenthetical), given W7 is the wave touching `same_substrate_union`.

**Minor REVISE 2:** Promote `REDRESS 96-98 PERMANENT pre-block` from parenthetical (SPEC.md:822) to a top-level pre-blocked-routes bullet in SPEC §10, with the verbatim P3-E §3 "No SK-V14 wave may force, amend, or split W3 to preserve the same union-substrate hypothesis" cite.

Other axes:
- Parallel substrate: NONE if §1.4 substrate-target triad is gate-consumed (per SPEC.md:807); the BackendShape dispatch is representation-replacement inside the singular retained `Tape` per SPEC.md:209.
- Sidecar producer: NONE (per SPEC.md:211 + §15 line 1083).
- Renamed scanner: NONE (W7 is dispatcher wiring).
- Track 1 ≡ Track 2 dishonesty: ACTIVELY PRE-BLOCKED via SPEC.md:806 + 810 (BackendShape dispatch with zero grammar-name arm).
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE per SPEC.md:211 + §15 line 1102.

**DISPOSITION: REVISE (minor; two clarifying-gloss + pre-block-promotion edits in SPEC §10).**

### §2.9 — W8 (SPEC §11 — CSS L4 Re-Admit R6)

Owner-paths (SPEC.md:835-844): `codegen/`, `runtime/src/grammars/css_l4_*/` (generated output), `bbnf-bench/src/css_l4_bench.rs`, `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, `REDRESS.md`. Behavior-LOC = 650 source/test.

Entry gate (SPEC.md:854): "W2 + W3 + W4 + W5 + W6 + W7 admitted (the full PRUNE chain + CSS L4 infrastructure)."

Per SPEC.md:856: "W8 plan does NOT include Stage-0 F-V2-P1ABC-RERECORD UNLESS it admits one of the 12 consumer-dependency primitives (per S-P2 V3 §6.3)."

Per SPEC.md:864: "If admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives, ship the rerun as Stage 0 of the same wave per S-P2 V3 §6.3."

**CH5 RISK 1 (REVISE):** Cross-checked against P3-B §2.12 (W9 wave-card; P3-B uses W9 numbering for CSS re-admit, SPEC uses W8 — SPEC's W8 = P3-B's W9), P3-B §2.12 line 260 names per-feature wave consumers including: **"P2-F C1 structural classify; P2-C C-P2C-1 ascii_set_member64_css_delimiter post-PRUNE-2 successor wiring; P2-F C5 string-block 64-byte oracle"**. P3-B §4 line 355 verbatim: "if W9's primitive consumers do NOT include the 12-list (i.e., W9 only admits CSS L4 primitives not in the 12-list), Stage 0 STILL ships in W9 because W9 is the first implementation wave per S-P2 §6.3 binding."

P3-B treats Stage 0 (F-V2-P1ABC-RERECORD) as **unconditional** in the first re-admit wave. SPEC §11 treats it as **conditional** ("UNLESS it admits one of the 12 consumer-dependency primitives"). **Per HARDENING-S-P2-V3-CONSOLIDATED §6.3 verbatim binding** (P3-A §1.1 line 26): "Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive" — and the **12-consumer-dependency list includes P2-F C6/C7/C10/C12/C13** (NOT C1; per p3a:26 verbatim "P2-A C6 + P2-C C-P2C-3/-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13").

Cross-checking the actual 12-list against the W8 consumer slate per P3-B §2.12 line 260:
- P2-F C1 structural classify → **NOT in the 12-list** (12-list is P2-F C6/C7/C10/C12/C13, NOT C1).
- P2-C C-P2C-1 ascii_set_member64_css_delimiter → NOT in the 12-list.
- P2-F C5 string-block 64-byte oracle → NOT in the 12-list.

So **W8's CSS re-admit consumer slate does NOT trigger the F-V2-P1ABC-RERECORD Stage-0 gate** under SPEC's "UNLESS" wording — SPEC is *consistent* with the actual 12-list. P3-B's "STILL ships" wording at §4 line 355 over-corrects.

**Per S-P2 V3 §6.3 binding verbatim** (HARDENING-S-P2-V3-CONSOLIDATED.md, cited at p3a:26 + p3b:33): "Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive". The dispatch envelope (`dispatch_value` at `runtime/src/grammars/json/generated.rs:45`) is masked only for JSON parse-only / direct planes, NOT CSS L4. So a CSS-only re-admit wave (W8) does NOT logically trigger the envelope-cracker requirement — the JSON dispatch envelope is not in the W8 owner-path family.

**HIDDEN COUPLING from this divergence:** if a P3-B reader expects Stage 0 unconditionally at W8, and SPEC reader sees the "UNLESS" deferral, the orchestrator dispatching W8 might ship one way or the other. SPEC is correct (Stage 0 conditional on 12-list); P3-B is over-cautious (Stage 0 unconditional). The inheritance-chain logic chains through W8 → W9 → W10 SPEC numbering, and the W10 admit slate (per P3-B §2.14 line 292 = SPEC W10 §13 line 980 task 2) consumes **P3-A C1 long-string SIMD = consolidates P2-E Gap 1 → IS in 12-list** → Stage 0 SHIPS at W10 unconditionally under SPEC's reading. The W10 task 5 SPEC.md:983 verbatim: "If admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives, ship the rerun as Stage 0 per S-P2 V3 §6.3" — verified.

**Minor REVISE 3:** SPEC §11 (W8) + SPEC §12 (W9) + SPEC §13 (W10) each carry an identical "UNLESS" deferral clause (SPEC.md:856 + 916 + 975). Add a corresponding clarifying note at each: "Per S-P2 V3 §6.3 + p3a:180 binding: the first wave admitting any of {C1 long-string SIMD via P2-E Gap 1, C3 digit-block via P2-E Gap 5 + C-P2C-3, C7 whitespace via P2-E Gap 3 + P2-F C7} MUST ship F-V2-P1ABC-RERECORD as Stage 0; downstream waves inherit; admission census per wave is the orchestrator's pre-dispatch verification step." This makes the chain of dependency explicit instead of relying on inference.

Other axes:
- Parallel substrate: NONE (consumes existing tape + bbnf-simd APIs).
- Sidecar producer: NONE.
- Renamed scanner: NONE.
- Track 1 ≡ Track 2 dishonesty: ACTIVELY PRE-BLOCKED per SPEC.md:866-871 (per-iter equality oracle; lightningcss + cssparser independent oracles).
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE; SPEC.md:881-886 pre-blocked routes verbatim include "single-quartet Unicode classifier + StringBlock16 tiny probe pre-blocks (REDRESS 82, 83)".

**DISPOSITION: REVISE (minor; three-instance Stage 0 clarifying-note edit at SPEC §11/§12/§13).**

### §2.10 — W9 (SPEC §12 — JSON Direct + Typed Re-Admit R7)

Owner-paths (SPEC.md:897-904): `bbnf-bench/benches/json_parity.rs`, `bbnf-bench/src/real_typed_struct.rs`, `codegen/` (only if generated path changes per plan). Behavior-LOC = 450 source/test.

- Parallel substrate: NONE (consumes existing direct + typed envelopes).
- Sidecar producer: NONE — explicit pre-block at SPEC.md:939-946 verbatim "sink-local decoded stats, quote-source streaming hash, direct source-hook folding, parser-owned scratch, byte-output unescape, semantic string fact hashing, raw f64 shortcut, stale canada mantissa widening, Track 2 coupling, direct cap-16 reruns, digest as typed product proof".
- Renamed scanner: NONE.
- Track 1 ≡ Track 2 dishonesty: ACTIVELY PRE-BLOCKED per SPEC.md:930 verbatim "Track 2 does not call generated SinkOnly, generated typed helpers, generated Track 1, or a shared benchmark-private parser; `track2_entry_point` column verifies symbol-path divergence".
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE per SPEC.md:939-946 verbatim.
- **NF-CH6-4 canonical-name binding compliance:** SPEC.md:944 verbatim "admitting three orthogonal SIMD bodies for the long-string-body scan primitive (S-P2 V3 §6.2 binding)" is PRE-BLOCKED.

**DISPOSITION: ACCEPT** (subject to §2.9 REVISE 3 Stage 0 clarifying note).

### §2.11 — W10 (SPEC §13 — JSON parse_only Distinct Path + Re-Admit R8)

Owner-paths (SPEC.md:957-964): `runtime/src/grammars/json/parser.rs` (stand up distinct parse_only path — no full-tape build per ORCHESTRATOR-PROMPT.md:147-149), `codegen/src/` (emit parse_only via W5 generator), `bbnf-bench/benches/json_parity.rs`, generated JSON `parse_only` output. Behavior-LOC = 650 source/test.

- Parallel substrate: GUARDED — the distinct parse_only path is per SPEC.md:986 exit-gate verbatim "Distinct parse_only code path exists in `generated_json`; no full-tape build". Per `[no-orthogonal-codepaths]` (cited at SPEC.md:980 task 2 + SPEC.md:1003 verbatim "orthogonal codepaths (conditional Vec-vs-scratch branching per `[no-orthogonal-codepaths]`)"): the parse_only emission is from ONE collection strategy (the W5 generator template parametrised by `parse_only=true`). The "no full-tape build" requirement is audit-checkable via tape-allocation telemetry (Lock 1 triad column).
- Sidecar producer: NONE (per SPEC.md:1002 + §15 line 1083).
- Renamed scanner: NONE (W5 generator template emits, not renames).
- Track 1 ≡ Track 2 dishonesty: ACTIVELY PRE-BLOCKED per SPEC.md:991 verbatim "Track 1 / Track 2 structural independence proven; `track2_entry_point` populated".
- Parser-owned projection / retained cursor / aux density / sidecar event vector: NONE per SPEC.md:1002-1004 verbatim "StringBlock16 tiny probe / object-pair value-byte control compaction (REDRESS 82-84); orthogonal codepaths".

The "no full-tape build" requirement is the strongest CH5 anti-parallel-substrate gate in the SPEC — the parse_only path consumes the `scan_structurals` positions Vec (existing substrate; `local_temp_only` per Lock 1 triad) without constructing a retained Tape. **VERIFIED at HEAD via `grep -n "fn scan_structurals" skinny/crates/runtime/src/grammars/json/scan.rs`.**

**DISPOSITION: ACCEPT** (subject to §2.9 REVISE 3 Stage 0 clarifying note).

### §2.12 — W11 (SPEC §14 — Close and Alpha Feedback)

Owner-paths (SPEC.md:1014-1020): `HANDOFF.md`, wave-11 close artifact under `research/`, optional `REDRESS.md` / `RESULTS.md` / `ROLLING-SOTA-DELTA.md`. Behavior-LOC = 0.

- Parallel substrate: N/A (close ceremony).
- Sidecar producer: N/A.
- Renamed scanner: N/A.
- Track 1 ≡ Track 2 dishonesty: PRE-BLOCKED carry — close ceremony cannot accept any row violating P-7 (SPEC.md:1051-1055 pre-blocked routes).
- Parser-owned projection / retained cursor / aux density / sidecar event vector: N/A.

**DISPOSITION: ACCEPT.**

## §3 — Cross-axis convergence verification

### §3.1 — Wave-numbering reconciliation (SPEC vs P3-B vs P3-A)

- P3-A §2.2 line 199: "**PRUNE-4 sub-wave count is 9 not 8** — P3-B per-wave sequencing must reflect this updated sub-wave count (per S-P0 A6)" — SPEC §9 W6 sub-wave table (SPEC.md:709-718) shows 9 sub-waves W6.1..W6.9; **CONVERGENT**.
- P3-B §2.1 line 73-85: W0..W11 12 waves; P3-B numbers PRUNE-1 = W2, R4 = W3, PRUNE-2 = W4, R5 = W5, PRUNE-3 = W6, PRUNE-4 = W7, PRUNE-5 = W8, R6 = W9, R7 = W10, R8 = W11.
- SPEC §2 (SPEC.md:236-248) wave manifest: SPEC numbers PRUNE-1 folded INTO W1 (W1 = R1+R2+PRUNE-1), R4 = W2, R5 = W3, PRUNE-2 = W4, PRUNE-3 = W5, PRUNE-4 = W6, PRUNE-5 = W7, R6 = W8, R7 = W9, R8 = W10, Close = W11.

**Wave-numbering divergence between P3-B and SPEC is 1 slot** (P3-B keeps PRUNE-1 as a separate W2; SPEC folds it into W1 alongside R1+R2). This divergence is consequential for the CHALLENGE-CONTEXT.md:21 cross-check (CH1 must verify SPEC §3-§14 matches one ordering exhaustively). **For CH5 substrate-union honesty, the divergence does not introduce hidden coupling** — both orderings preserve the substrate-before-consumer topology (substrate waves W0-W7 SPEC numbering / W0-W8 P3-B numbering before any admit wave). But for the orchestrator's dispatch envelope, the divergence is a CH1 reconcile-target.

**CH5 disposition: ACCEPT for hidden-coupling; flag for CH1 lens cross-reference.**

### §3.2 — Lock 1 v+1 substrate-target triad gate-consumption verification

Per SPEC.md:223 non-negotiable verbatim:

> Lock 1 v+1: every e-graph candidate, backend rewrite, imported scanner plan, union candidate, and SIMD consumer declares `substrate_target` ... `retention_lifetime` ... `policy_owner` ... `xtask gate-json` rejects any row whose REDRESS lacks the triple.

Per SPEC.md:153 telemetry verbatim:

> substrate_target / retention_lifetime / policy_owner  (required per Lock 1 v+1 for any wave admitting a SIMD/union/cost-shape consumer; allowed values per LOCKS.md:76-82)

Per P3-D §2.1 row 24 + row 25 line 56-57 verbatim:

> **`structural_projection_status`** ... Empty → reject; W3 "side substrate" leak → reject per SK-V8 §0.4 line 145. **`substrate_cardinality`** ... Empty → reject; substrate union breach (Lock 1 — two live substrates on the same row) → reject per SYNTHESIS §0.4 P-5.

Per P3-C §3.6 line 296 verbatim:

> Per-shape Lock-1 triad declared in REDRESS per SYNTHESIS §4 line 317-319: `substrate_target`, `retention_lifetime`, `policy_owner` triple populated for every SIMD consumer wired by C-4; `xtask gate-json` rejects any row whose REDRESS lacks the triple.

The Lock 1 v+1 triad is **gate-consumed at three sites**: SPEC.md:223 non-negotiable + SPEC.md:153 telemetry + P3-D §2.1 schema + P3-C §3.6 W7 gate. The orphan-emission risk (column emitted without gate consumer) is structurally pre-blocked. **CH5 disposition: ACCEPT.**

### §3.3 — Track 1 ≡ Track 2 dishonesty census across all admit waves (W8 + W9 + W10)

Three admit waves carry the CH5 gate:
- W8 SPEC §11 + P3-C §3.7 line 331 (CSS L4 features; `track2_entry_point` ≠ Track 1 ancestor beyond Tape/OffsetFlags).
- W9 SPEC §12 + P3-C §3.8.2 line 368 + P3-C §3.9.2 line 401-402 (JSON direct + typed).
- W10 SPEC §13 + P3-C §3.10 line 437 (JSON parse_only).

The gate is consistent across all three admit waves. Test instrument is named at P3-D §3.5 line 116: "artificially point Track 2 at a private tape internal and confirm rejection". **CH5 disposition: ACCEPT.**

### §3.4 — F-V2-P1ABC-RERECORD inheritance chain across W8 → W9 → W10

Per p3a:180 + p3b:33: "first wave admitting any of {C1, C3, C7} MUST ship C5 as Stage 0".

W8 owner-path family (SPEC.md:838-844) is CSS L4 → admits ZERO of {C1, C3, C7} per P3-B §2.12 candidate slate (P2-F C1, C-P2C-1, P2-F C5 — none are P3-A C1/C3/C7).
W9 owner-path family (SPEC.md:898-902) is JSON direct + typed → admits ZERO of {P3-A C1, C3, C7} per P3-B §2.13 candidate slate (P2-A C1 lazy-field-skip is P3-A C2 not C1; P2-C C-P2C-4 is P3-A C4; P2-D C-P2D-1 is P3-A C8 — none are C1/C3/C7).
W10 owner-path family (SPEC.md:957-964) is JSON parse_only → **admits P3-A C1 long-string SIMD** via the parse_only distinct path consumer (per P3-B §2.14 line 292 candidate slate "P2-A C2 / P2-E Gap 1 / P2-F C1+C2 = ONE canonical long-string-body SIMD scan primitive per S-P2 §6.2" — this IS P3-A C1).

So per p3a:180 binding, **the first wave admitting C1 = W10 SPEC §13**. F-V2-P1ABC-RERECORD Stage 0 MUST ship in W10. SPEC §13 line 983 task 5 verbatim: "If admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives, ship the rerun as Stage 0 per S-P2 V3 §6.3." Verifying the 12-list inclusion: P2-E Gap 1 is in the 12-list (p3a:26 + p3b:33 verbatim "P2-E Gap 1"); P3-A C1 consolidates Gap 1. So C1 admission DOES trigger the "UNLESS" deferral → Stage 0 SHIPS at W10. **SPEC §13 is internally consistent.**

P3-B §4 line 355 contradicts: "if W9's primitive consumers do NOT include the 12-list (i.e., W9 only admits CSS L4 primitives not in the 12-list), **Stage 0 STILL ships in W9 because W9 is the first implementation wave per S-P2 §6.3 binding**." P3-B uses W9 numbering for the first re-admit wave = SPEC W8 = CSS L4 re-admit. So P3-B asserts: **Stage 0 ships at the first re-admit wave regardless of 12-list inclusion**, because the envelope-cracker is a first-implementation-wave commitment.

SPEC's reading ("UNLESS it admits one of the 12 consumer-dependency primitives") is WEAKER than P3-B's reading ("first implementation wave unconditionally"). **CH5 RISK 4 (REVISE — already counted under §2.9 REVISE 3):** P3-B and SPEC disagree on Stage 0's unconditionality at W8. Per S-P2 V3 §6.3 verbatim binding cited at p3a:26: "**Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive**" — the binding is over "dispatch-envelope-internal" primitives, NOT a generic "first implementation wave". The JSON dispatch envelope (`dispatch_value` at `runtime/src/grammars/json/generated.rs:45`) is NOT exercised by W8 CSS L4 re-admit — therefore SPEC is correct and P3-B is over-strict.

**Reconciliation:** SPEC is internally consistent (Stage 0 conditional on 12-list admission; W8 admits none of 12-list; defer); P3-B is over-cautious (Stage 0 unconditional at first re-admit; redundant for CSS-only W8). **CH5 disposition: ACCEPT SPEC's reading; flag for V2 cross-axis reconciliation between P3-A and P3-B on Stage 0 unconditionality semantics.**

### §3.5 — Substrate-union honesty across cross-cutting Stage-0 Lock-15 + LTO consumers

C6 `force_inline_lto_envelope_discipline` (p3a:126-138; P3-A §2 candidate) is a build-invariant, not a substrate primitive. Per p3a:130 verbatim: "Scalar reference exists by construction (the envelope IS scalar — the existing `dispatch_value` at `crates/runtime/src/grammars/json/generated.rs:45` is the scalar shape)." C6 ships **PAIRED with C5** per p3a:137 verbatim: "PAIRED with C5 — the verification (cargo asm + samply re-record) requires the `parse-attribution` feature ON for the per-primitive attribution; ships in the same wave as C5."

C5 Stage 0 wave-of-first-admit = W10 SPEC §13 (per §3.4 above). Therefore **C6 ships at W10 alongside C5**. C6 is build-invariant; introduces ZERO substrate; zero CH5 risk. **CH5 disposition: ACCEPT.**

### §3.6 — Lock 1 v+1 "imported scanner plan" gate (SK-V14 NEW Lock 16 binding)

Per SPEC.md:225 non-negotiable verbatim:

> Lock 16 v+1: every `core::arch::*`, `target_feature`, and `asm!` use-site in `bbnf-simd`, parse-that facades, generated scanners, or collapsed-stage code maps to a manifest row containing stable primitive id, abstract primitive name, primary ISA/library citation, hardware gate, scalar reference, strict checkasm/parity command (`BBNF_SIMD_STRICT=1`), corpus/equality parity, grammar policy source, substrate target, retention lifetime, policy owner, same-wave production consumer, expected row/feature gate, LOC/risk, rollback path, abrogate threshold, and final disposition.

Each shortlist SIMD primitive (C1, C3, C4, C7) carries: scalar-ref status PRESENT + checkasm-parity EXTENSION + same-wave-consumer NAMED per p3a §2 entries. Per Lock 16 v+1 manifest the substrate-target / retention-lifetime / policy-owner triad is REQUIRED. **VERIFIED across the four SIMD primitives in p3a §2 / §2.1 line 171-178.**

**Imported scanner plan** semantics per Lock 1 v+1 verbatim (LOCKS.md:73): "Every e-graph candidate, backend rewrite, **imported scanner plan**, union candidate, and SIMD consumer must declare `substrate_target`, `retention_lifetime`, and `policy_owner`." The shortlist primitives are all *internally authored* in `bbnf-simd/src/aarch64/`, not *imported* from external scanners (no simdjson scanner port; no sonic-rs scanner port; no lightningcss scanner port). The CH5 risk surface "imported scanner with un-declared substrate target" is ZERO across the shortlist. **CH5 disposition: ACCEPT.**

## §4 — Disposition summary

| Axis | Verdict | Notes |
|---|---|---|
| §1.1 Parallel substrate introduction | ACCEPT | 8/8 candidates declare substrate_target per Lock 1 v+1; two (C2, C8) are retention-subtractive |
| §1.2 Sidecar producer / aux density / sidecar event vector | ACCEPT | SPEC §1 lines 211-212 + §15 line 1102 verbatim forbid; P3-E §3 PERMANENT pre-blocks honoured |
| §1.3 Renamed scanner (Lock 1 spirit) | ACCEPT | No-rename clause amended 2026-05-04; canonical-name binding consolidates 3 → 1 (opposite of rename) |
| §1.4 Track 1 ≡ Track 2 dishonesty | ACCEPT | `track2_entry_point` CH5 gate wired at telemetry + all 3 admit waves; rejection rule testable |
| §1.5 Parser-owned structural projection / retained cursor | ACCEPT | SPEC.md:211 verbatim forbids; C2/C8 explicitly retention-subtractive |
| §1.6 NF-CH6-4 canonical-name binding | ACCEPT | 5-witness convergence (SPEC §1 + §15 + P3-A §2.1 + P3-A C1 + P3-B §1.1 + §4) |
| §2.1 W0 telemetry-install wave | ACCEPT | Installs CH5 instruments |
| §2.2 W1 comparator + PRUNE-1 | ACCEPT | Harness-only |
| §2.3 W2 regen-css xtask | ACCEPT | xtask emission only |
| §2.4 W3 production CSS corpora | ACCEPT | Corpora bytes only |
| §2.5 W4 PRUNE-2 CSS template delete + 24-row revert | ACCEPT | Retention-subtractive |
| §2.6 W5 PRUNE-3 trait-dispatch + generic generator | ACCEPT | Structural refactor |
| §2.7 W6 PRUNE-4 9-sub-wave per-grammar runtime collapse | ACCEPT | Pattern H opt-out enshrinement rewrite is positively retention-subtractive |
| §2.8 W7 PRUNE-5 wire W8 policy + W9 union | **REVISE (minor; 2 clarifying edits)** | `same_substrate_union` naming proximity to SK-V9 W3 retired hypothesis; REDRESS 96-98 needs promotion from parenthetical to top-level pre-block bullet in SPEC §10 |
| §2.9 W8 CSS L4 re-admit | **REVISE (minor; 1 clarifying note)** | Stage 0 conditional clause needs explicit inheritance-chain note distinguishing 12-list admission vs first-implementation-wave commitment |
| §2.10 W9 JSON direct + typed re-admit | ACCEPT (with §2.9 inherited REVISE) | NF-CH6-4 pre-block honoured |
| §2.11 W10 JSON parse_only distinct path | ACCEPT (with §2.9 inherited REVISE) | "No full-tape build" + `[no-orthogonal-codepaths]` are the strongest CH5 anti-parallel-substrate gates |
| §2.12 W11 close ceremony | ACCEPT | P-7 carry; close cannot accept violations |
| §3.1 Wave-numbering reconciliation | ACCEPT (flag for CH1) | 1-slot SPEC vs P3-B divergence (PRUNE-1 fold); not CH5-load-bearing |
| §3.2 Lock 1 v+1 triad gate-consumption | ACCEPT | 3-site gate consumption |
| §3.3 Track 1 ≡ Track 2 census across admit waves | ACCEPT | Gate consistent across W8 + W9 + W10 |
| §3.4 F-V2-P1ABC-RERECORD inheritance chain | ACCEPT (with cross-axis flag) | SPEC's reading internally consistent; P3-B over-cautious; reconciliation for V2 |
| §3.5 C6 Lock-15 + LTO substrate-union | ACCEPT | Build-invariant; ships paired with C5 at W10 |
| §3.6 Lock 16 v+1 imported scanner plan | ACCEPT | All SIMD primitives internally authored; zero imported-scanner risk |

**Cycle disposition:** **REVISE** (3 minor clarifying edits in SPEC §10 + identical 3-instance Stage 0 inheritance note at SPEC §11/§12/§13). NO REJECTs. NO load-bearing hidden-coupling risk surfaced.

**ACCEPT-rate:** 24 ACCEPT / 27 axes evaluated = **24/27 = 88.9 %**. Three REVISE entries are minor clarifying edits, NOT material structural reworks; the underlying substrate-union honesty is preserved across every wave.

## §5 — Findings detail (the three REVISE prescriptions)

### §5.1 — REVISE 1 (SPEC §10 W7; `same_substrate_union` clarifying gloss)

**Location:** `restart/skinny/tranches/sk-v14/SPEC.md:799` (W7 task 2 verbatim "Wire `same_substrate_union` (W9 SCAFFOLD per ORCHESTRATOR-PROMPT.md:124-126) into compile + lower + runtime paths.").

**Proposed insertion (after task 2):**

> Note: `same_substrate_union` is the W9 SCAFFOLD enforcement module per `ORCHESTRATOR-PROMPT.md:124-126`; it ENFORCES Lock 1 substrate-union honesty (singular `runtime::tape` consumer per shape; CSP-emitted `BackendShape` dispatch). It is NOT a `UnionTape`, NOT a class-column union, NOT a parallel substrate. SK-V9 W3 retired union-substrate hypothesis (REDRESS 96/97/98) is permanently pre-blocked per `LOCKS.md:84-90` + SPEC §15 line 1102; no SK-V14 wave may force, amend, or split W3 to preserve the same union-substrate hypothesis (per P3-E §3 verbatim Item 96 line 196-210).

**Rationale:** the W7 module name `same_substrate_union` is the *enforcement* module per ORCHESTRATOR-PROMPT.md:124-126, not the audit-falsified data structure from SK-V9 W3. The naming proximity is a CH5 hidden-coupling read-failure risk: an orchestrator reading SPEC §10 in isolation could conflate the enforcement module with the retired retained-class-column-union hypothesis. The gloss eliminates the ambiguity.

### §5.2 — REVISE 2 (SPEC §10 W7; REDRESS 96-98 pre-block promotion)

**Location:** `restart/skinny/tranches/sk-v14/SPEC.md:822` (W7 pre-blocked routes verbatim, currently parenthetical: "(UnionTape-style retained structures per Lock 1 v+1).").

**Proposed promotion to top-level bullet (insert before line 822):**

> - **REDRESS 96-98 PERMANENT pre-block** (full class-column vectors; streaming structural cursors; class-lane-only replays; parser-owned sidecars; UnionTape-style retained structures per `LOCKS.md:84-90` v+1 substrate-ceiling history). Per P3-E §3 verbatim binding: "No SK-V14 wave may force, amend, or split W3 to preserve the same union-substrate hypothesis without first ...". W7 wires the enforcement module per ORCHESTRATOR-PROMPT.md:124-126; W7 does NOT reopen REDRESS 96/97/98.

**Rationale:** W7 is the wave touching `same_substrate_union`; REDRESS 96/97/98 are PERMANENT pre-blocks (per P3-E §3.1 + LOCKS.md:84-90). The current SPEC §10 carries them only as a parenthetical at line 822; promotion to a top-level pre-blocked-route bullet matches the discipline at W4 SPEC §7 (REDRESS 60-72 explicit pre-block) and W9 SPEC §12 line 939-946 (REDRESS 66-72, 80 watch-list explicit).

### §5.3 — REVISE 3 (SPEC §11 + §12 + §13 — Stage 0 inheritance chain clarifying note)

**Locations:**
- SPEC.md:856 (W8 §11): "W8 plan does NOT include Stage-0 F-V2-P1ABC-RERECORD UNLESS it admits one of the 12 consumer-dependency primitives (per S-P2 V3 §6.3)."
- SPEC.md:916 (W9 §12): "W9 plan does NOT include Stage-0 F-V2-P1ABC-RERECORD UNLESS it admits one of the 12 consumer-dependency primitives."
- SPEC.md:975 (W10 §13): "W10 plan does NOT include Stage-0 F-V2-P1ABC-RERECORD UNLESS it admits one of the 12 consumer-dependency primitives."

**Proposed insertion (after each of the three "UNLESS" clauses):**

> Inheritance-chain note: per `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:180` + `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.3`: the binding is over "dispatch-envelope-internal primitives" (P2-A C6 + P2-C C-P2C-3/-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13 — the 12-list verbatim); P3-A C1 long-string SIMD consolidates P2-E Gap 1 → admit IS in 12-list; P3-A C3 digit-block consolidates C-P2C-3 + Gap 5 → admit IS in 12-list; P3-A C7 whitespace SIMD consolidates Gap 3 + P2-F C7 → admit IS in 12-list. The first wave admitting any of {P3-A C1, C3, C7} ships Stage 0 unconditionally; downstream waves inherit. Orchestrator pre-dispatch verification step: enumerate the wave's S-P2 LOCKED primitive consumers; cross-check against the 12-list; if intersection is non-empty AND Stage 0 has not yet shipped on a prior W{N<current}, ship Stage 0 as Stage 0 of the current wave.

**Rationale:** the "UNLESS" clauses at SPEC §11/§12/§13 are internally consistent but inferentially complex. The reader must trace: (a) what's the wave's primitive consumer slate? (b) what's the 12-list? (c) does (a) ∩ (b) ≠ ∅? (d) has Stage 0 shipped on a prior wave? (e) if (c)=YES and (d)=NO, ship Stage 0 now. The clarifying note makes this 5-step inference explicit, eliminating the inheritance-chain read-failure risk that P3-B §4 line 355 over-corrected for ("Stage 0 STILL ships in W9 because W9 is the first implementation wave"). Both readings converge once the inheritance chain is surfaced.

## §6 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:134-138` (CH5 lens definition verbatim)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md:32` (V1 disposition focus + §2.Y NF-CH6-4 binding)
- `restart/locks/LOCKS.md:48-90` (Lock 1 verbatim + 2026-05-04 no-rename amendment + 2026-05-21 v+1 substrate-ceiling fold + REDRESS 96/97/98 binding)
- `restart/locks/LOCKS.md:73-82` (Lock 1 v+1 substrate-target/retention-lifetime/policy-owner triad)
- `restart/locks/LOCKS.md:265-281` (Lock 15 LTO + force-inline + i-cache ≤ 20 KiB)
- `restart/locks/LOCKS.md:282-340` (Lock 16 SIMD/ASM allowlist + abstract-primitive declarations)
- `restart/skinny/tranches/sk-v14/SPEC.md:1-1137` (S-P3 V1 SPEC verbatim; lines cited per disposition row)
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:1-344` (triumvirate contract; no CH5-load-bearing edits identified)
- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:1-317` (shortlist + substrate-target census + 12-list verbatim binding)
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md:1-406` (wave manifest + cross-wave pre-blocks + Stage 0 inheritance)
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md:1-526` (per-wave gates + P-7 Track 1 ≡ Track 2 enforcement)
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:1-167` (`track2_entry_point` CH5 hidden-coupling gate + rejection rule)
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md:1-902` (REDRESS 96/97/98 PERMANENT pre-block census + SK-V9 W3 union-substrate intrinsic block)
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md:1-244` (V1 dispatch SPEC drafting notes)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.1-§6.3` (CF-3 3-gate + NF-CH6-4 + F-V2-P1ABC-RERECORD Stage-0 carry-forward)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §0.4 P-7` (Track 1 ≡ Track 2 dishonesty pattern pre-block)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §2 line 240` (`track2_entry_point` column rule)
- HEAD verification commands:
  - `grep -n "fn scan_string_special_block" skinny/crates/bbnf-simd/src/aarch64/string_block.rs` (C1 canonical scalar-ref existence)
  - `grep -n "fn scan_structurals" skinny/crates/runtime/src/grammars/json/scan.rs` (C2 + C7 existing substrate)
  - `grep -n "fn skip_value" skinny/crates/bbnf-bench/src/generated_real_typed.rs` (C2 rewrite target)
  - `grep -n "track2_entry_point\|same_substrate_union\|UnionTape\|class-lane" restart/skinny/tranches/sk-v14/SPEC.md` (verbatim citation chain validation)
