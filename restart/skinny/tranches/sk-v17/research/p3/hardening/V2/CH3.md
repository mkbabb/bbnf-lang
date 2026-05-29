# SK-V17 P3 CHALLENGE — CH3 REGRESSION (V2)

Lens: CH3 REGRESSION. Cycle: V2. Date: 2026-05-29.
Pass: S-P3 Synthesis-Plan CHALLENGE (PASS-3 §3 CH3, ORCHESTRATOR §3W).
Master HEAD: `f87ee713a` (`git rev-parse HEAD = f87ee713a7cf82e6d2cc82738dde313940c49121`).
Subjects reviewed: `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md`,
`restart/skinny/tranches/sk-v17/SPEC.md`.
Focus (per dispatch): no wave re-opens a REDRESS pre-block; PRUNE-before-rebuild
order honored; the fact-stream / W5C retirement does not strand a consumer;
JSON 51/51 guarded; the P3-E ledger correctly enumerates every per-wave pre-block;
the SPEC carries the full inherited family list.

## §0 — Verdict summary

| Wave / section | Disposition | Note |
|---|---|---|
| Inherited family list (SPEC §9 `:825-827`) | ACCEPT | full list present: 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV |
| Second-substrate guard (SPEC §9 `:802-806`; Lock 1) | ACCEPT | grep-clean at HEAD (verified) |
| W0 (baseline) | ACCEPT | dirty-regen re-keyed to W5; no-generated-change explicit (`p3e:96-102`, `SPEC.md:367`) |
| W1 (PRUNE / tape) | ACCEPT | full stranded-consumer set enumerated + tree-verified; exceeds V1 ask |
| W2 (projection) | ACCEPT | L8 in W2 table; index-as-parallel-vector + sidecar blocks present |
| W3 (NEON) | ACCEPT | PMULL/CTZ/lo6/orphan-kernel blocks aligned |
| W4 (L9, conditional) | ACCEPT | post-W1 re-profile hard gate present |
| W5 (close) | ACCEPT | close-wave table now present (was the V1 REJECT) |
| PRUNE-before-rebuild order | ACCEPT | W1 prunes, W2 rebuilds (`SPEC.md:392-394`, `p3b:22-24`) |
| JSON 51/51 guard | ACCEPT | ±1.0% floor now distinguished from -2.0% CSS maintain |
| Internal `:669-719` citation drift (P3-E `:177`, SPEC §9 internal) | REVISE | cosmetic: W5 section actually starts `SPEC.md:725` (SPEC grew when W1 enum folded) |

Counts: ACCEPT 10 · REVISE 1 · REJECT 0.

## §1 — V1 dispositions all folded (the load-bearing V1 defect is resolved)

The V1 CH3 found a 5-wave ledger keyed against a 6-wave SPEC, an absent W5 close-wave
row (REJECT), L8 mis-keyed to W1, dirty-regen mis-keyed to W0, and an under-enumerated
W1 consumer-strand. **Every one is folded in V2:**

1. **[V1 REJECT → ACCEPT] W5 close-wave table now present.** P3-E §2 carries a full
   W5 subsection (`p3e-preblocked-ledger.md:176-195`) enumerating dirty-generated close
   (REDRESS §0.4 last bullet, SK-V15 W7-W11 `DifferentFile("generated.rs")`), paper close,
   legacy-shim deletion before proof, brace-counter-as-close-surrogate, and corpus-average
   substituting per-corpus medians. The SPEC W5 §8 carries the same set
   (`SPEC.md:766-769`). The 5-vs-6 wave desync is gone: P3-E §1 mapping table
   (`p3e:73-80`) is now the canonical W0–W5 six-wave scheme keyed to `SPEC.md:257-267`.

2. **[V1 REVISE → ACCEPT] L8 re-keyed W1→W2.** P3-E now files the sparse-flag→sidecar
   and L8-hand-curated-catalogue pre-blocks in the W2 table (`p3e:134`), matching the
   SPEC L8-in-W2 placement (`SPEC.md:469-470`); the W1 table explicitly states "**L8 is
   NOT a W1 candidate** — it lands in W2" (`p3e:122`). SPEC §9 W2 attribution carries
   "L8 flag as hand-curated per-rule catalogue … retained cursor / aux density / sidecar
   event vector" (`SPEC.md:820`).

3. **[V1 REVISE → ACCEPT] dirty-regen re-keyed W0→W5.** P3-E W0 table explicitly bars
   "any generated `generated.rs` regen (the dirty-regen close is a **W5** gate, not W0)"
   (`p3e:96`) and the closing line "The dirty-generated-close `regen --check` 9/9 gate is
   **re-keyed to W5**" (`p3e:101-102`). SPEC W0 §3 lands 0 behavior LOC / no generated
   change (`SPEC.md:367`); the `regen --check` 9/9 gate is the W5 exit gate
   (`SPEC.md:743,752`).

4. **[V1 REVISE → ACCEPT, exceeded] W1 consumer-strand fully enumerated AND tree-verified.**
   This was the V2 dispatch's load-bearing focus. The V1 finding named
   `lib.rs:581,597,1001,1035,1109,1113` as additionally stranded. V2 goes further and
   correctly enumerates the FULL stranded set across all three crates. I verified each
   against the tree at HEAD `f87ee713a`:
   - `codegen/src/lib.rs`: `:299` selected, `:336` array def, `:567,:611` array iteration,
     `:581,:1001,:1035` `.contains("emit_fact_stream")` assertions, `:597,:1109,:1113`
     `w5c_*` round-trip + source-hash consumers — ALL present in tree (grep confirmed).
   - `runtime/src/lib.rs:76,91,108,126,143,162,434`: the SEVEN `css_l4_*_emit_fact_stream`
     round-trip test fns — ALL present (grep confirmed).
   - `grammars/css_l4_*/parser.rs:6`: the SEVEN live `generated::emit_fact_stream(input)`
     entries — ALL present across the 7 grammar dirs (grep confirmed).
   - `runtime_generator.rs:621,666,694`: the `emit_fact_stream` generator template — present.
   The SPEC W1 §4 owner-path block (`SPEC.md:403-424`) and task 6 (`SPEC.md:453-459`)
   enumerate every one of these and bind the same-wave-consumer obligation: "NO
   `emit_fact_stream` round-trip assertion may dangle after the wave: a surviving consumer
   asserting a String round-trip strands a retired plane and FAILS the wave"
   (`SPEC.md:456-459`). P3-E mirrors this in the W1 stranded-consumer pre-block
   (`p3e:113`) and the §3 grep gate "no dangling `emit_fact_stream` round-trip assertion =
   `grep -n 'emit_fact_stream\|W5C_REQUEST_FACT_PROFILES' codegen/src/lib.rs` returns zero
   after the W1 commit" (`p3e:267-269`). **The retirement strands no consumer: every
   consumer site is named, with a same-commit migrate-or-delete obligation and a
   greppable exit gate. This focus is fully discharged.**

5. **[V1 REVISE → ACCEPT] JSON guard citation aligned.** P3-E §3 now cites the JSON
   guard floor at the strict ±1.0% ("51/51 JSON rows strict, same-plane, cold",
   `p3e:246-248`; the -2% W10b idiom is reserved for the per-row tripwire, `p3e:252`).
   No reader will mistake -2.0% for the JSON floor.

## §2 — Per-wave pre-block disposition (all ACCEPT)

### W0 — ACCEPT
Pre-blocks: broadcast 215 (`p3e:93`), fact-stream comparator (`p3e:94`), warm/single-sample
(`p3e:95`), no-behaviour/no-generated-change (`p3e:96`). All cited and wave-correct. W0
lands no L*, no generated change (`SPEC.md:367`); the dirty-regen gate is correctly W5.

### W1 — PRUNE / fact-stream + W5C retirement → tape — ACCEPT
PRUNE-before-rebuild explicit: W1 "DELETES the fact-stream String plane and the hand-coded
routing array, THEN routes CSS into the existing tape — it does not add a parallel tape
path beside the String" (`SPEC.md:392-394`). Pre-blocks correct and cited: union-substrate
96/97/98 RETIRED (`p3e:108`, admissible only via the REDRESS-140 differential, cardinality
one), AZ-IV eager (`p3e:109`), StructRegistry indirection (`p3e:110`), W5C
relocated-not-retired (`p3e:111`), fact-stream as admission plane (`p3e:112`), the
stranded-consumer obligation (`p3e:113`, full set), `split_off`/`Vec<Vec>` (`p3e:114`). No
re-opened route. Same-wave consumer = L3 minimal cursor in the same W1 commit
(`p3e:116-120`). ACCEPT.

### W2 — layout-driven lazy projection generator — ACCEPT
Pre-blocks correct: JSON `value_from_ref` rider re-emit byte-equal (the CH2 anti-CSS-pinned
gate, `p3e:132`), parser-local structural cursor 51/53 (`p3e:133`), L8 sparse-flag→sidecar
+ L8-hand-curated-catalogue (`p3e:134`, re-keyed from W1), fake-generated-template 213
(`p3e:135`), eager/preserve-rich-ast (`p3e:136`), second substrate via
`StructLayout`/`TapeCursor` (`p3e:137`). SPEC §9 W2 attribution (`SPEC.md:820`) carries the
L8-catalogue + REDRESS-53 + sidecar blocks. ACCEPT.

### W3 — NEON structural index — ACCEPT
Pre-blocks correct and SPEC-aligned: union-substrate re-emergence admissible only as
transient index == tape offsets (`p3e:151`), PMULL on hot path (REDRESS 88, `p3e:152`,
L5 uses `escape_mask_64` carry idiom not PMULL), CTZ as default body (REDRESS 89,
`p3e:153`, L6 default = scalar running balance), lo6/`classify_tbl4` on CSS (`;{`→slot-59
`& 0x3f` collision, eq-set fan instead, `p3e:154`), orphan kernel / net-new without
checkasm + same-wave consumer (REDRESS 88/89, SK-V5 W5, udot/i8mm barred, `p3e:155`),
micro-kernel without same-row gate + re-profile obligation (REDRESS 80/82-84, `p3e:156`).
SPEC §9 W3 attribution (`SPEC.md:821`) matches. The §4 barred set (orphan udot, net-new
i8mm, FNV/hex, asmjson FSM, lo6-on-CSS, D6 second substrate) matches the LOCKED
ineligibility (`p3e:288-306`, `SPEC.md:847-849`). ACCEPT.

### W4 — L9 commit-by-construction (conditional) — ACCEPT
Post-W1 re-profile hard gate present (`p3e:167`, `SPEC.md:669-675,690-692`): the
28.87%+2.45% recognition-control figures are NOT a measured rollback antecedent; L9 admits
only if a post-W1 N≥50 typed-tape re-profile surfaces a top-N rollback/recognition-control
leaf, else not-dispatched (honest non-admission). `split_off`/`Vec<Vec>` (`p3e:168`) and
value-discard (`p3e:169`) blocks present. ACCEPT.

### W5 — close wave — ACCEPT (was the V1 REJECT)
The W5 close-wave table (`p3e:176-195`) now enumerates: dirty-generated close /
hand-patched generated (`p3e:186`, SK-V15 W7-W11 `DifferentFile("generated.rs")`), paper
close (`p3e:187`), legacy-shim deletion before replacement proof (`p3e:188`,
PRUNE-before-rebuild — deletion follows the proof), brace-counter proof as a close
surrogate (`p3e:189`, the EXACT 8-field `rules=10136, style=9561, sel=9561, decls=20043`
re-proof), corpus-average substituting per-corpus medians (`p3e:190`). SPEC W5 §8
pre-blocked routes (`SPEC.md:766-769`) and §9 W5 attribution (`SPEC.md:823`) carry the
same set. ACCEPT.

## §3 — JSON 51/51 guard — ACCEPT

Well-defended across the cohort. SPEC §0.1 row 1, §0.5 floor (±1.0% across every behavior
wave), per-wave maintain rows in §2.5 (`SPEC.md:297-301`: each of W1/W2/W3/W4 carries
"JSON 51/51 maintain"), and each wave exit gate re-asserts it. P3-C gates it per-wave; the
re-entry signature (a JSON row dropping below its locked floor — twitter 17685,
citm_catalog 28630, `REDRESS.md:2828`) is named as the falsifier (`p3e:246-252`). The V1
citation-looseness note is resolved: P3-E §3 now cites the strict ±1.0% JSON floor and
reserves -2.0% for the CSS recognition-plane maintain (`p3e:246-252`). ACCEPT.

## §4 — PRUNE-before-rebuild order — ACCEPT

W1 is the PRUNE wave (DELETE fact-stream + W5C, THEN route into the existing tape; no
parallel path — `SPEC.md:392-394`). W2 is the rebuild (the full rich projection generator
generalizing W1's minimal cursor — `SPEC.md:466-468` family). W3 (NEON) is gated behind W1
tape activation (no structural index to scan into until the tape decodes CSS — SYNTHESIS
§0.1 NEON gate; `p3b:48,60-63`). P3-B states the order explicitly: "the V1 W1
(tape+projection merged) is split into SPEC W1 (PRUNE/tape) + SPEC W2 (layout-driven
projection)" (`p3b:22-24`). Topological order (telemetry → prune → rebuild → NEON → L9 →
close) holds. No wave rebuilds on an un-pruned base. ACCEPT.

## §5 — No wave silently re-opens a pre-blocked route — ACCEPT

Grep of `skinny/crates/` at HEAD `f87ee713a` for `struct StructLayout|struct TapeCursor|
struct UnionTape|TapeStructBuilder|struct StructRegistry` returns ZERO — the Lock-1
no-second-substrate guard is honored at the current base, and the SPEC forbids introducing
them (SPEC §9 global block `:802-806`; per-wave W2 `:820`). No SPEC wave's owner paths or
tasks introduce a renamed scanner, a parallel substrate, a sidecar producer, or a retained
cursor. The cohort now speaks ONE wave numbering (P3-A/P3-B/P3-E/SPEC all W0–W5,
L2/L7/L3-min in W1, L3-full/L8/L4 in W2, L1/L5/L6 in W3, L9 in W4, close in W5 — the V1
root-cause three-scheme desync is resolved). ACCEPT.

## §6 — The single residual: stale internal `:669-719` line citation — REVISE (cosmetic)

The ONLY remaining defect is a stale internal line-reference. P3-E W5 (`p3e:177`) and
several P3-E/SPEC internal citations refer to the W5 close section as `SPEC.md:669-719`.
At the V2 HEAD that range is actually the W4 §7 L9 wave (`SPEC.md:669-723`); the W5 close
§8 begins at **`SPEC.md:725`** and runs to `:775`. The SPEC grew ~50 lines when V2 folded
the full W1 consumer enumeration (`SPEC.md:403-424,453-459`), shifting every section below
W1 downward, but the inherited V1 internal citations were not re-numbered.

This is a citation-drift defect, not a content defect: the W5 close-wave content IS
present and correct (`SPEC.md:725-775`), the pre-blocks ARE enumerated, and no wave
re-opens a route. It is REVISE (cosmetic), not REJECT.

**Concrete fix:** in `p3e-preblocked-ledger.md`, update the W5 subsection citation
(`:177` "the close wave (`SPEC.md:669-719`)") to `SPEC.md:725-775`; sweep P3-E for any
other `:669`/`:687`/`:696`/`:710-713` W5 citations and re-key to the W5 §8 block
(`regen --check` 9/9 is now `SPEC.md:743,752`; the close-time pre-blocks are
`SPEC.md:766-769`; dirty-generated close `SPEC.md:767`). Likewise sweep the SPEC's own
internal cross-references (e.g. `SPEC.md:669-719` appearing inside §9 or §7) and re-point
to the post-fold line numbers. A line-anchored citation that points at the wrong section
is a maintenance hazard for the wave triumvirate reading the ledger by line.

## §7 — Folding directives for V3

ONE disposition to fold (all V1 dispositions already folded; convergence achieved on the
substantive axes):

1. **[REVISE → citation re-key]** Re-key the stale `SPEC.md:669-719` W5 citation in
   `p3e-preblocked-ledger.md:177` (and any sibling `:687/:696/:710-713` W5 refs) to the
   post-fold W5 §8 block: close section `SPEC.md:725-775`, `regen --check` 9/9 gate
   `:743,752`, close-time pre-blocks `:766-769`. Sweep the SPEC for self-citations of the
   same stale range. Cosmetic; no content change.

No other CH3 defect remains. The pre-block ledger is complete and wave-correct; no wave
re-opens a route; PRUNE-before-rebuild holds; the fact-stream/W5C retirement strands no
consumer (every site named + tree-verified + same-commit migrate/delete obligation +
greppable exit gate); JSON 51/51 is guarded ±1.0% across every wave; the inherited family
list is full.

## §8 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH3, §8.5 same-wave-consumer.
- `restart/skinny/tranches/sk-v17/SPEC.md`: §2 manifest (`:264-269`), §2.5 maintain budget
  (`:296-301`), §3 W0 no-generated-change (`:367`), §4 W1 PRUNE owner paths + full
  consumer enumeration (`:392-459`), §7 W4 L9 conditional (`:669-723`), §8 W5 close
  (`:725-775`), §9 route ledger + per-wave attributions + inherited families
  (`:777-849`).
- `restart/skinny/tranches/sk-v17/research/p3/p3e-preblocked-ledger.md`: §1 mapping
  (`:64-80`), W0 (`:89-102`), W1 (`:104-126`), W2 (`:128-145`), W3 (`:147-161`), W4
  (`:163-174`), W5 (`:176-195`), inherited families (`:197-238`), §3 falsifiability
  (`:240-279`), §4 barred set (`:281-315`).
- `restart/skinny/tranches/sk-v17/research/p3/p3b-wave-sequencing.md`: 6-wave reconciliation
  (`:22-24,48,60-63`), wave manifest (`:121-130`).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V1/CH3.md` (V1 dispositions folded).
- `skinny/REDRESS.md`: 96-98 retirement, 140 differential (`:4245-4252`), JSON floors
  twitter 17685 / citm_catalog 28630 (`:2828`).
- `skinny/crates/codegen/src/lib.rs` consumers verified `:299,336,567,581,597,611,1001,
  1035,1109,1113` (grep at HEAD).
- `skinny/crates/runtime/src/lib.rs` consumers verified `:76,91,108,126,143,162,434` (grep).
- `skinny/crates/runtime/src/grammars/css_l4_*/parser.rs:6` (7 dirs, grep).
- `skinny/crates/codegen/src/runtime_generator.rs:621,666,694` (grep).
- Owner-path grep: `skinny/crates/` clean of `struct StructLayout|struct TapeCursor|
  struct UnionTape|TapeStructBuilder|struct StructRegistry` at HEAD `f87ee713a`.
