# SK-V9 S-P2 CHALLENGE V3 — CH2 GENERALITY

Pass: S-P2 Research. Cycle: V3. Lens: CH2 GENERALITY (Lock 14).
Date: 2026-05-18.
Predecessor: `restart/skinny/tranches/sk-v9/research/p2/hardening/V2/CH2.md`
(100% — 54 ACCEPT, 0 REVISE, 0 REJECT; first qualifying cycle).
Fold spec: `HARDENING-S-P2-V2-CONSOLIDATED.md` "V3 fold requirements" —
eight surgical single-sentence edits across P2-D + P2-F (five P2-D,
three P2-F). Anchor: `restart/locks/LOCKS.md:60` Lock 14 verbatim.
Canonical primitive-class vocabulary: `skv9-p1-v3-B-xctrace-time-
profiler.md` §1.5 (seven-class taxonomy).

This document is the V3 re-CHALLENGE: it verifies the V2 hygiene
residual (R-CH2-V2-1) folded, that the eight V3 surgical edits
introduced no new Lock-14 leak — in particular no JSON-only conclusion
restated as a substrate-primitive fact — and that primitive-class tags
remain consistent with the P1-V3-B §1.5 canonical vocabulary. V3 is the
candidate **second-consecutive** qualifying cycle per `ORCHESTRATOR.md`
§3Z (≥95% × 2 consecutive).

---

## §1 — V2-residual resolution

### §1.1 — R-CH2-V2-1: P2-F P1-V3-B §1.5 anchor by path — RESOLVED

V2 §4 carried one non-blocking hygiene residual, unchanged since V1
§4 item 7 (V1's own designation: "Cohort discipline; non-blocking"):
P2-F did not anchor the P1-V3-B §1.5 canonical primitive-class
vocabulary *by path*; it inherited the vocabulary by reference through
P2-A / P2-D / P2-E. The V2 consolidation lifted this into the V3 fold
list as item 8 ("P2-F — anchor P1-V3-B §1.5 canonical vocabulary by
path").

V3 disposition: **FOLDED — clean.** P2-F §5.4-preamble (the asmjson
architectural-pattern bullet, lines 310–315) now reads: "the
`BYTE_CLASS_FROM_EQ_SET_64` + `BITMAP_NEXT_SET_BIT` +
`FSM_DISPATCH_THREADED` primitive vocabulary (per SK-V7-A2 §6; the
canonical primitive-class taxonomy this report inherits is anchored at
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-
profiler.md` §1.5)." The anchor is now a verbatim path, placed in the
load-bearing surface where P2-F first names a primitive vocabulary. The
P2-F §0 V3-fold footer (lines 684–686) records the edit explicitly
against "CH2 item-7 hygiene." This closes R-CH2-V2-1; the cohort is now
uniform — P2-A anchors §1.5 inline (line 292, §2.5), P2-D carries the
§1.5 `Class` column through its §2 tables and anchors P1-V3-B §2 +
§3.5 by path, and P2-F now anchors §1.5 by path. **No CH2 residual
carries into V3.**

The anchor is reference-only — it cites the *vocabulary source*, not a
JSON conclusion. The three named symbols (`BYTE_CLASS_FROM_EQ_SET_64`,
`BITMAP_NEXT_SET_BIT`, `FSM_DISPATCH_THREADED`) are asmjson's
architectural primitives cited as cross-ISA architecture-pattern
evidence; none is asserted as a bbnf-substrate behaviour. No leak.

---

## §2 — V3 dispositions

The V3 fold touched only P2-D and P2-F (eight single-sentence edits).
P2-A / P2-B / P2-C / P2-E carry no V3 edit; their V2 ACCEPT dispositions
stand unchanged and are re-confirmed here as a block (§2.1). The eight
V3-edited surfaces each carry a dedicated new-leak check (§2.2 P2-D,
§2.3 P2-F).

### §2.1 — Unchanged-report re-confirmation (P2-A/B/C/E)

| # | Report | V2 aggregate | V3 status |
|---:|---|---|---|
| U.1 | P2-A union event-model — 9 ACCEPT (incl. A.5 JSON-role naming, A.6 `json_templates/` carve-out, A.7–A.9 new-leak). No V3 edit touches P2-A. | ACCEPT | **UNCHANGED — ACCEPT** |
| U.2 | P2-B retained grammar proof — 9 ACCEPT (incl. B.5 `AnyGrammar` uninhabited FactId, B.7–B.9 new-leak). No V3 edit touches P2-B. | ACCEPT | **UNCHANGED — ACCEPT** |
| U.3 | P2-C Apache + CITM admission — 9 ACCEPT (incl. C.4/C.5 cross-grammar prose + Track-2 oracle, C.7–C.9 new-leak). No V3 edit touches P2-C. | ACCEPT | **UNCHANGED — ACCEPT** |
| U.4 | P2-E `escape_codec_hex_unit` codec — 9 ACCEPT (incl. E.4 scaffold/production taxonomy, E.7/E.8 new-leak). No V3 edit touches P2-E. | ACCEPT | **UNCHANGED — ACCEPT** |

The V3 fold spec confines all eight edits to P2-D and P2-F; a
file-scope check of the two reports' §0 V3 footers confirms no edit
crossed into P2-A/B/C/E. The 36 V2 dispositions on these four reports
stand. No spot-check finds a primitive-class tag drift: P2-A still
names walker functions in the generic-template form with JSON codegen
output scoped as per-grammar realisation; P2-B's `AnyGrammar` still
carries the uninhabited `FactId`; P2-C's §5.1 cross-grammar table is
still tagged illustrative-not-in-scope; P2-E's three-way
production/scaffold/no-consumer taxonomy is intact.

### §2.2 — P2-D V3-edited surfaces (five edits)

| # | Surface | Verdict |
|---:|---|---|
| D.V3-1 | §5.3.1 six-row no-regression maintain gate (CH3 D-1/D-10) — §5.3.1 gains the W10b six-row maintain gate mirroring §4.4's CSSC CTZ slice. New-leak check: the six rows (canada, citm_catalog, … per §5.3.1) are *JSON corpus rows*; verify the gate is framed as a per-primitive regression gate, not a JSON-substrate claim. The gate guards the `bitmap_prefix_xor_64` primitive (a grammar-neutral structural-bitmap producer, §5 P2-A scope) against the W10b regression profile; the corpus rows are the *measurement vehicle*, not a substrate behaviour. The primitive remains grammar-neutral; the gate is a checkasm/bench discipline. No leak. | **ACCEPT** |
| D.V3-2 | §5.3.1 EOR3 latency cite (CH6 V2-D-8) — the SHA3 `veor3q_u8` latency claim now cites ARM DDI 0487 FEAT_SHA3 / FEAT_PMULL with the M5-Max-unpublished caveat. New-leak check: the cite is an ISA-document reference for an arm64 NEON ternary-bitwise primitive (Lock 16 allowlist entry, "arm64 NEON ternary bitwise"). §5.3.1's surrounding "Lock 16 admissibility caveat" still gates the EOR3 body behind `FEAT_SHA3=1` and keeps the scalar shift-XOR ladder as the unconditional fallback. The primitive is grammar-neutral; the citation names no grammar. No leak. | **ACCEPT** |
| D.V3-3 | §6.3 wording (CH6 V2-D-6) — §6.3 reworded to distinguish the per-primitive checkasm tests (same-wave admission preconditions per §6.2.1) from the deferred host-instrumentation infrastructure (invariants 2-5). New-leak check: the reword is a process-discipline clarification; §6.3 + §6.2.1 keep the checkasm gate *per-primitive*, not per-grammar — every missing differential is assigned to the wave that broadens/widens/wires the primitive. No grammar-specific gate introduced. No leak. | **ACCEPT** |
| D.V3-4 | §5.5 / §8 REDRESS 28+33 line ranges (CH1 LOW REVISE) — REDRESS 28 + 33 citations now carry `skinny/REDRESS.md` line ranges (28 → `:324-337`, 33 → `:394-418`). New-leak check: a citation-precision edit; the cited REDRESS entries are pre-block records, not substrate claims. CH1 lens owns the line-range correctness; CH2 verifies only that the edit added no grammar-specific surface — it did not. No leak. | **ACCEPT** |
| D.V3-5 | §0 footer cascade-sequencing note — the §0 footer records the four "block on P2-A OR fail CH5" slices create a wave-sequencing constraint S-P3 must honour. New-leak check: a process note for S-P3; it names no grammar, asserts no substrate behaviour. The four slices guard grammar-neutral primitives behind the P2-A union-substrate dependency. No leak. | **ACCEPT** |

P2-D V3: 5 ACCEPT, 0 REVISE, 0 REJECT.

Note on D.V3-4: the V2 consolidation's V3-fold-requirements item 7
named the line ranges as `1241-1278 / 1314-1343`, whereas the folded
P2-D §0/§5.5/§8 prose uses `:324-337` / `:394-418`. This is a CH1-lens
discrepancy (citation correctness against the live `REDRESS.md`), not
a CH2 surface — flagged here for the CH1 V3 auditor, recorded as a
cross-lens observation in §4. It is not a Lock-14 leak and does not
gate the CH2 verdict.

### §2.3 — P2-F V3-edited surfaces (three edits)

| # | Surface | Verdict |
|---:|---|---|
| F.V3-1 | §5.2 inline REDRESS-33 cite (CH3 fold #2) — §5.2's sonic-rs `match_tiny_plain_string`-class lesson now inline-cites that its dispatch-site NEON wiring shape is pre-blocked by `skinny/REDRESS.md` entry 33 (`:394-418`). New-leak check: the edit *strengthens* Lock-14/REDRESS hygiene — it explicitly demotes a SOTA-pattern observation from "admission" to "lesson," and the lesson concerns the `match_tiny_plain_string` primitive (Class A, Lock-16-admitted, grammar-neutral). §5.2 names twitter/update_center/apache_builds/github_events as *expected-impact corpora*, not as substrate behaviours; the dispatch-site wiring is bound to a REDRESS-33 material-differential gate. No JSON-only conclusion presented as a primitive fact. No leak. | **ACCEPT** |
| F.V3-2 | §2.1 ContainerNext code cite + §5.4 CollapsedStage anchor (CH6 V2-F-5) — the §2.1 ContainerNext reference now carries the `generated.rs:341` enum-definition cite (consumed at `:134-135`, emitted by `consume_array_next` at `:348-375`); the §5.4 CollapsedStage reference is anchored to its `restart/ARCHITECTURE.md` §7.3 design-corpus definition. New-leak check: `ContainerNext` / `consume_array_next` are JSON codegen-output symbols — the cite points at `skinny/crates/runtime/src/grammars/json/generated.rs`, which is the **per-grammar JSON codegen-output directory** (the Lock-14-permitted (c)-surface, parallel to `crates/<grammar>/`), NOT a generic-substrate path. The §2.1 prose names ContainerNext as the V9.5 Wave-2-close JSON realisation that eliminates per-element re-dispatch — a JSON-codegen-output fact correctly scoped to the JSON grammar's emitted module, not asserted as a `runtime/src/tape/` substrate behaviour. CollapsedStage is anchored to ARCHITECTURE.md §7.3 as the fifth grammar-neutral `BackendShape` variant (`LayoutFacts.backend_shape`), a side-table field — not a JSON-specific type. No leak. | **ACCEPT** |
| F.V3-3 | §5 asmjson P1-V3-B §1.5 anchor (CH2 item-7 hygiene = R-CH2-V2-1) — see §1.1. The asmjson primitive-vocabulary reference now anchors the canonical primitive-class taxonomy by path to `skv9-p1-v3-B-xctrace-time-profiler.md` §1.5. New-leak check: a vocabulary-source citation; the three named symbols are asmjson architectural primitives cited as cross-ISA pattern evidence, none asserted as a bbnf substrate behaviour. Closes the last CH2 residual. No leak. | **ACCEPT** |

P2-F V3: 3 ACCEPT, 0 REVISE, 0 REJECT.

### §2.4 — Spot-check: ≥15 dispositions, primitive-class consistency

Beyond the eight V3-edited surfaces (D.V3-1..5, F.V3-1..3) the V3
audit re-spot-checks ≥15 dispositions for primitive-class tag
consistency and cross-grammar admission resolution:

| # | Spot-check | Result |
|---:|---|---|
| S.1 | P2-A A.5 — `walk_container_at_class` generic template; JSON `parse_object`/`parse_array` scoped as per-grammar codegen output. | Consistent — class ordinal generic, JSON symbol per-grammar. |
| S.2 | P2-A A.6 — `json_templates/` carve-out as per-grammar codegen-instance directory, parallel to `crates/<grammar>/`. | Consistent — (c)-surface, not substrate. |
| S.3 | P2-B B.5 — `AnyGrammar` uninhabited `FactId`, `STRUCTURAL_CLASS_COUNT = 0`. | Consistent — empty-grammar identity, not JSON default. |
| S.4 | P2-B B.4 — `*_witness/` directory carve-out; JSON witness at `grammars/json/event_grammar_witness.rs`. | Consistent — `_witness` marker on file (JSON) / directory (no-parser grammars). |
| S.5 | P2-C C.4 — `<grammar>_real_typed_struct` row id form, `sk-v{N>9}-<grammar>-real-typed-w{n}` schema identity. | Consistent — grammar-neutral methodology shape. |
| S.6 | P2-C C.5 — `serde_json` named JSON-specific oracle; structural-independence is the grammar-neutral invariant. | Consistent — oracle shape grammar-dependent, invariant grammar-neutral. |
| S.7 | P2-D D.1 — §2 per-row `Class` column drawn from P1-V3-B §1.5 seven-class taxonomy. | Consistent — class tags match canonical vocabulary. |
| S.8 | P2-D D.3 — `scan_string_special_block_32` named as Lock-14 primitive-vocabulary entry; four `.data`-slot parameters. | Consistent — primitive grammar-neutral, JSON first consumer. |
| S.9 | P2-D D.4 — §5.3.1 `veor3q_u8` per Lock 16 "arm64 NEON ternary bitwise" allowlist entry. | Consistent — abstract-primitive lift, grammar-neutral. |
| S.10 | P2-D §4 — `match_string_at_quote_trusted_utf8` named as per-grammar JSON realisation of the string-block primitive. | Consistent — JSON consumer scoped, kernel grammar-neutral. |
| S.11 | P2-D §6.2.1 — checkasm tests (`checkasm_unescape_uxxxx.rs` etc.) per-primitive, not per-grammar. | Consistent — per-primitive gate discipline. |
| S.12 | P2-E E.5 — `escape_codec/` grammar-neutral primitive directory; per-binding files named by parameter (`hex_x4`, `hex_x8`, `hex_variable`). | Consistent — primitive-directory naming grammar-neutral. |
| S.13 | P2-E E.6 — `surrogate_join_policy = Pair` a const-generic parameter codegen emits for JSON, constant-folds out elsewhere. | Consistent — parameter binding, not `match grammar` arm. |
| S.14 | P2-E E.4 — three-way taxonomy: 1 production consumer (JSON x4 path), 2 scaffolds (CSS L4 + TOML). | Consistent — scaffold/production distinction intact. |
| S.15 | P2-F F.3 — §5.3 yyjson fusion framed as `escape_codec_hex_unit` class; codegen-emitted. | Consistent — codec class tag matches §1.5/§3.5 vocabulary. |
| S.16 | P2-F F.V3-2 — ContainerNext at `grammars/json/generated.rs:341`; cross-grammar admission resolves to JSON per-grammar codegen output. | Consistent — JSON codegen-output directory, (c)-surface. |
| S.17 | P2-F §5.4 — CollapsedStage as fifth `BackendShape` variant per ARCHITECTURE.md §7.3. | Consistent — grammar-neutral side-table field. |
| S.18 | P2-D §5 — `bitmap_prefix_xor_64` primitive sited in `bbnf-simd/src/aarch64/`, exercised by structural-bitmap producer (P2-A scope). | Consistent — grammar-neutral SIMD crate, no grammar arm. |
| S.19 | P2-F §5.2 — Class A `match_tiny_plain_string` named "Lock-16-admitted" primitive; the wiring (not the primitive) is REDRESS-33 pre-blocked. | Consistent — primitive grammar-neutral; wiring site is the gated surface. |
| S.20 | P2-D D.2 — §3.4 cross-grammar unicode-escape codec parameter table mirroring P1-V3-B §3.5; CSS L4 / JS `\u{...}` / TOML `\U` rows. | Consistent — cross-grammar admissions resolve to per-grammar `.data` parameter rows. |

20 spot-checks, all consistent. Every primitive-class tag traces to the
P1-V3-B §1.5 seven-class taxonomy or the §3.5 codec parameter table;
every cross-grammar admission (S.5, S.6, S.13, S.14, S.16, S.20)
resolves to a per-grammar declarative surface — grammar source,
workspace metadata, or per-grammar codegen-output directory — never to
a generic-crate `match grammar` arm, grammar-named generic module, or
grammar-specific generic public type.

### §2.5 — V3 disposition count

13 dispositions in §2: 5 P2-D V3-edited surfaces (D.V3-1..5) + 3 P2-F
V3-edited surfaces (F.V3-1..3) + 4 unchanged-report block re-confirms
(U.1–U.4) + 1 V2-residual resolution (§1.1). Plus 20 spot-checks
(§2.4). Total CH2 V3 verdict-bearing items: 14 (9 disposition rows +
4 block re-confirms + 1 residual), all ACCEPT; 0 REVISE; 0 REJECT.

Carrying the four unchanged reports at their full V2 row counts (the
36 untouched V2 ACCEPT rows on P2-A/B/C/E) plus the 18 V3-cohort rows
(8 V3-edited + 10 P2-D/P2-F V2 rows re-confirmed unchanged) yields a
cohort total of **54 dispositions, 54 ACCEPT, 0 REVISE, 0 REJECT** —
the V3 cohort holds the V2 count with the eight edited surfaces all
clearing.

---

## §3 — Aggregate verdict

| Report | ACCEPT | ACCEPT-WITH-REVISE | REJECT | Aggregate |
|---|---:|---:|---:|---|
| P2-A union event-model | 9 | 0 | 0 | ACCEPT (unchanged from V2) |
| P2-B retained grammar proof | 9 | 0 | 0 | ACCEPT (unchanged from V2) |
| P2-C Apache + CITM admission | 9 | 0 | 0 | ACCEPT (unchanged from V2) |
| P2-D aarch64 ASM opportunities | 10 | 0 | 0 | ACCEPT (5 V3-edited surfaces clear) |
| P2-E `escape_codec_hex_unit` codec | 9 | 0 | 0 | ACCEPT (unchanged from V2) |
| P2-F SOTA teardown M5 Max | 8 | 0 | 0 | ACCEPT (3 V3-edited surfaces clear) |
| **Total** | **54** | **0** | **0** | **ACCEPT** |

ACCEPT rate: 54 / 54 = **100%** (V1: 80.6%; V2: 100%).

CH2 GENERALITY verdict on the S-P2 V3 cohort: **ACCEPT.**

The V2 hygiene residual R-CH2-V2-1 is folded — P2-F now anchors the
P1-V3-B §1.5 canonical primitive-class vocabulary by path, and the
cohort is uniform on the §1.5 anchor. The eight V3 surgical edits are
all single-sentence, confined to P2-D (five) and P2-F (three), and
none introduces a new Lock-14 leak: none restates a JSON-only
conclusion as a substrate-primitive fact. F.V3-2's `generated.rs:341`
cite points at the JSON per-grammar codegen-output directory — the
Lock-14-permitted (c)-surface — not a generic crate; F.V3-1's
REDRESS-33 inline cite strengthens hygiene by explicitly demoting a
SOTA pattern from "admission" to "lesson"; F.V3-3 closes the residual.
The five P2-D edits are a regression-gate addition, two ISA/REDRESS
citations, a process-discipline reword, and a cascade-sequencing
note — all grammar-neutral.

The 20 spot-checks confirm primitive-class tag consistency against the
P1-V3-B §1.5 seven-class taxonomy and §3.5 codec parameter table, and
confirm every cross-grammar admission resolves to a per-grammar
declarative surface.

**Convergence.** V3 CH2 returns 100% ACCEPT. Combined with V2's 100%,
this is the **second consecutive** cycle clearing the ≥95% bar
(ORCHESTRATOR.md §3Z: ≥95% × 2 consecutive). **CH2 GENERALITY
converges at V3.** No V3 finding blocks S-P2 CH2 convergence.

---

## §4 — Any new Lock-14 leaks

**No new Lock-14 leak introduced by the V3 fold.** All eight surgical
edits are grammar-neutral; the three new-leak checks on the P2-F edits
and the five on the P2-D edits (§2.2, §2.3) pass. No CH2 residual
carries into a V4 — R-CH2-V2-1 is closed.

One cross-lens observation, recorded for the CH1 V3 auditor (not a
Lock-14 leak, not a CH2 surface, does not gate the CH2 verdict):

- **O-CH2-V3-1 (cross-lens, CH1).** The V2 consolidation's
  V3-fold-requirements item 7 named the REDRESS 28+33 line ranges as
  `1241-1278 / 1314-1343`. The folded P2-D §0 footer, §5.5, and §8
  use `:324-337` (entry 28) and `:394-418` (entry 33). The two
  numbers disagree. This is a citation-correctness question against
  the live `skinny/REDRESS.md` — CH1's lens, not CH2's. The CH1 V3
  auditor should verify which line range is correct against the
  current `REDRESS.md`. The discrepancy is not a grammar-specific
  surface and introduces no Lock-14 leak; it is logged here only so
  the CH1 lens does not miss it.

Two admissibility facts from the V2 verdict carry forward unchanged
into the S-P3 plan (neither is a leak; both were made explicit by the
V2 fold and survive V3 untouched):

- **`json_templates/` carve-out is the cohort's standing position.**
  S-P3 must not silently re-open the rename; the carve-out (per-grammar
  codegen-instance directory ≡ Lock-14 (c)-surface) is load-bearing
  cohort prose.
- **P2-E `hex_x8_neon` (TOML) lands with no production consumer this
  wave.** S-P3 must carry the "TOML production consumer wires in a
  later wave" deferral explicitly so the kernel does not read as orphan
  code in the wave diff.

End of disposition. CH2 GENERALITY: **ACCEPT at V3 — converged
(second consecutive ≥95%).**
