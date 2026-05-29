# SK-V17 P3 CHALLENGE — CH3 REGRESSION (V3)

Lens: CH3 REGRESSION. Cycle: V3. Date: 2026-05-29.
Pass: S-P3 Synthesis-Plan CHALLENGE (PASS-3 §3 CH3, ORCHESTRATOR §3W).
Master HEAD: `f87ee713a` (`git rev-parse HEAD = f87ee713a7cf82e6d2cc82738dde313940c49121`).
Subjects reviewed: `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md`,
`restart/skinny/tranches/sk-v17/SPEC.md`.
Focus (per dispatch): no wave re-opens a REDRESS pre-block; PRUNE-before-rebuild order
honored; the fact-stream / W5C retirement strands no consumer; JSON 51/51 guarded; the
P3-E ledger correctly enumerates every per-wave pre-block; the SPEC carries the full
inherited family list.

## §0 — Verdict summary

| Wave / section | Disposition | Note |
|---|---|---|
| V2 residual (stale `:669-719` W5 citation) | ACCEPT | resolved — grep of `669/687/696/710-713/669-719` over `p3e` returns NONE |
| Inherited family list (SPEC §9 `:830-832`) | ACCEPT | full list present: 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV |
| Second-substrate guard (SPEC §9 `:807-811`; Lock 1) | ACCEPT | grep-clean at HEAD (verified ZERO) |
| W0 (baseline) | ACCEPT | no-behaviour/no-generated-change explicit (`SPEC.md:375`); dirty-regen re-keyed to W5 (`p3e:101-102`) |
| W1 (PRUNE / tape) | ACCEPT | full stranded-consumer set enumerated + tree-verified at HEAD; same-commit migrate/delete obligation + greppable gate |
| W2 (projection) | ACCEPT | L8 in W2 table; JSON `value_from_ref` byte-equal re-emit + sidecar blocks present; -2.0% maintain band distinguished from ±1.0% JSON floor |
| W3 (NEON) | ACCEPT | PMULL/CTZ/lo6/orphan-kernel/96-97-98 blocks aligned to REDRESS ids |
| W4 (L9, conditional) | ACCEPT | post-W1 re-profile hard gate; 28.87%+2.45% NOT a rollback antecedent; honest non-dispatch recorded |
| W5 (close) | ACCEPT | close-wave pre-block table present; dirty-regen 9/9 + paper-close + legacy-shim-before-proof + brace-counter + corpus-average all enumerated |
| PRUNE-before-rebuild order | ACCEPT | W1 prunes, W2 rebuilds (`SPEC.md:392-394`, `p3b:31,38`); topological |
| JSON 51/51 guard | ACCEPT | ±1.0% floor across every behavior wave; -2.0% reserved for W2 CSS-typed maintain |
| REDRESS-140 differential (96/97/98 admissibility seam) | ACCEPT | cited verbatim; cardinality one, index == tape offsets |

Counts: ACCEPT 12 · REVISE 0 · REJECT 0.

## §1 — The single V2 residual is folded; convergence on the CH3 axis

The V2 CH3 returned ACCEPT 10 · REVISE 1 · REJECT 0, the lone REVISE being a stale
internal line citation (`SPEC.md:669-719` pointing at the W4 §7 L9 wave instead of the
W5 §8 close). **It is folded in V3.** A grep of `p3e-preblocked-ledger.md` for any of
`669 | 687 | 696 | 710-713 | 669-719` returns NONE — the W5 close section is now cited
by SPEC §-anchor and by its post-fold line range. I re-verified the canonical SPEC
section anchors at HEAD:

- §3 W0 `:337`, §4 W1 PRUNE `:390`, §5 W2 projection `:494`, §6 W3 NEON `:588`,
  §7 W4 L9 `:668`, §8 W5 close `:730`, §9 route ledger `:782`.
- The `regen --check` 9/9 dirty-regen gate is `SPEC.md:748,757` (P3-E cites exactly these).
- P3-E's W2 owner-path citations (`:494-583`, `:534-536,550-556`, `:515-516,542-543,576-577`),
  W3 (`:588-666`), W4 (`:668-723`), W5 (`:730-779`, `:748,757`) all resolve to the correct
  post-fold sections. No citation-drift defect remains.

## §2 — Per-wave pre-block disposition (all ACCEPT)

### W0 — ACCEPT
Pre-blocks: 24-row broadcast 215 (`p3e:93`, `SPEC.md:380-382`), fact-stream comparator
(`p3e:94`), warm/single-sample (`p3e:95`), no-behaviour/no-generated-change (`p3e:96`).
All cited and wave-correct. SPEC W0 lands "NO parser/scanner/SIMD/codegen behavior or
generated parser output change" (`SPEC.md:375`); the dirty-regen `regen --check` 9/9 gate
is correctly re-keyed to W5 (`p3e:101-102`, `SPEC.md:748,757`), not W0. Same-wave consumer
is the `gate-json` row consumer (no kernel ships at W0, `SPEC.md:377`).

### W1 — PRUNE / fact-stream + W5C retirement → tape — ACCEPT
PRUNE-before-rebuild explicit: W1 "DELETES the fact-stream String plane and the hand-coded
routing array, THEN routes CSS into the existing tape — it does not add a parallel tape
path beside the String" (`SPEC.md:392-394`). Pre-blocks correct and cited: union-substrate
96/97/98 RETIRED (`p3e:108`, admissible ONLY via the REDRESS-140 differential — verified
verbatim at `REDRESS.md:4245-4252`: "no public `UnionTape` … retained structural index,
sidecar vector, parser-owned cursor, second scan … Substrate cardinality remains one"),
AZ-IV eager (`p3e:109`), StructRegistry indirection (`p3e:110`), W5C relocated-not-retired
(`p3e:111`), fact-stream as admission plane (`p3e:112`), the stranded-consumer obligation
(`p3e:113`, full set), `split_off`/`Vec<Vec>` (`p3e:114`). No re-opened route.

**Stranded-consumer enumeration — tree-verified at HEAD `f87ee713a`** (the dispatch's
load-bearing focus). The fact-stream/W5C retirement strands no consumer; every named site
is present in the tree and bound to a same-commit migrate/delete obligation:
- `codegen/src/lib.rs`: `W5C_REQUEST_FACT_PROFILES` at `:299` (selected), `:336` (array
  def), `:567,:611` (array iteration) — grep-confirmed exact; `.contains("emit_fact_stream")`
  assertions at `:581,1001,1035` — grep-confirmed exact; the `w5c_*` request round-trip +
  source-hash consumers at `:597,1109,1113` — grep-confirmed (1109/1113 inside
  `w5c_gen_css_runtime_output_depends_on_frontend_source_hash` `:1107`). P3-E `:113` additionally
  lists `:569,:613` (the `emit_runtime_from_request(w5c_*)` calls inside the iteration loops) —
  both confirmed present. The enumeration is correct and slightly OVER-complete vs the SPEC
  owner-path subset, which is the safe direction for a retirement ledger.
- `runtime/src/lib.rs`: the SEVEN `css_l4_*_emit_fact_stream` round-trip test fns at
  `:76,91,108,126,143,162,434` — grep-confirmed exact.
- `grammars/css_l4_*/parser.rs:6`: SEVEN live `generated::emit_fact_stream(input)` entries
  across all seven grammar dirs — grep-confirmed exact.
- `runtime_generator.rs:621,666,694`: the `emit_fact_stream` generator template — confirmed
  (`:621` template call, `:666` doc, `:694` the `pub fn emit_fact_stream` itself).

The SPEC W1 owner-path block (`SPEC.md:403-424`) and task 6 (`SPEC.md:453-459`) enumerate
every one of these and bind the same-wave-consumer obligation: "NO `emit_fact_stream`
round-trip assertion may dangle after the wave: a surviving consumer asserting a String
round-trip strands a retired plane and FAILS the wave" (`SPEC.md:456-459`). The greppable
exit gate is `SPEC.md:467-471` + `p3e:267-272` (grep over BOTH `runtime/src/lib.rs` and
`codegen/src/lib.rs` returns ZERO surviving String-round-trip consumers after the W1
commit). **This focus is fully discharged: every consumer site named, tree-verified,
same-commit obligation, greppable gate.** ACCEPT.

### W2 — layout-driven lazy projection generator — ACCEPT
Pre-blocks correct: JSON `value_from_ref` rider re-emit byte-equal (the CH2 anti-CSS-pinned
gate, `p3e:133`, `SPEC.md:534-536,550-556`), parser-local structural cursor 51/53 (`p3e:134`),
L8 sparse-flag→sidecar + L8-hand-curated-catalogue (`p3e:135`, re-keyed from W1 — matches
SPEC L8-in-W2 placement `SPEC.md:515-516,542-543,576-577`), fake-generated-template 213
(`p3e:136`), eager/preserve-rich-ast (`p3e:137`), second substrate via
`StructLayout`/`TapeCursor` (`p3e:138`). SPEC §9 W2 attribution (`SPEC.md:825`) carries
"L8 flag as hand-curated per-rule catalogue; L1/L4 index as parallel retained vector
(REDRESS-53); retained cursor / aux density / sidecar event vector". The V3 W2 maintain
budget at §5 (`SPEC.md:564-568`) correctly sets the CSS-typed median floor at -2.0% vs the
W1 baseline — distinct from the ±1.0% JSON floor — folding the V2→V3 disposition. ACCEPT.

### W3 — NEON structural index — ACCEPT
Pre-blocks correct and REDRESS-id-aligned: union-substrate re-emergence admissible only as
transient index == tape offsets (`p3e:152`), PMULL on hot path (REDRESS 88, `p3e:153`,
L5 uses `escape_mask_64` carry idiom not PMULL), CTZ as default body (REDRESS 89, `p3e:154`,
L6 default = scalar running balance), lo6/`classify_tbl4` on CSS (`;{`→slot-59 `& 0x3f`
collision, eq-set fan instead, `p3e:155`), orphan kernel / net-new without checkasm +
same-wave consumer (REDRESS 88/89, SK-V5 W5, udot/i8mm barred, `p3e:156`), micro-kernel
without same-row gate + re-profile obligation (REDRESS 80/82-84, `p3e:157`). SPEC §9 W3
attribution (`SPEC.md:826`) matches. §4 barred set (orphan udot, net-new i8mm, FNV/hex,
asmjson FSM, lo6-on-CSS, D6 second substrate) matches the LOCKED ineligibility
(`p3e:291-309`, `SPEC.md:852-854`). ACCEPT.

### W4 — L9 commit-by-construction (conditional) — ACCEPT
Post-W1 re-profile hard gate present (`p3e:168`, `SPEC.md:668-692`): dispatches ONLY if a
post-W1 N≥50 typed-tape re-profile surfaces a top-N rollback/recognition-control leaf; the
28.87%+2.45% recognition-control figures are explicitly NOT a measured rollback antecedent;
if the re-profile does not fire, W4 does NOT dispatch (honest non-admission, recorded per
`abrogate-before-patch`, `SPEC.md:680-682`). `split_off`/`Vec<Vec>` (`p3e:169`) and
value-discard (`p3e:170`) blocks present. ACCEPT.

### W5 — close wave — ACCEPT
The W5 close-wave table (`p3e:185-191`) enumerates: dirty-generated close / hand-patched
generated (`p3e:187`, SK-V15 W7-W11 `DifferentFile("generated.rs")`), paper close
(`p3e:188`), legacy-shim deletion before replacement proof (`p3e:189`, PRUNE-before-rebuild
— deletion follows the proof), brace-counter proof as a close surrogate (`p3e:190`, the
EXACT 8-field `rules=10136, style=9561, sel=9561, decls=20043` re-proof), corpus-average
substituting per-corpus medians (`p3e:191`). SPEC W5 §8 pre-blocked routes
(`SPEC.md:771-774`) and §9 W5 attribution (`SPEC.md:828`) carry the same set. The
`regen --check` 9/9 dirty-regen gate is `SPEC.md:748,757`. ACCEPT.

## §3 — JSON 51/51 guard — ACCEPT

Well-defended across the cohort. SPEC §1 row 1 (`:42-44`), the §-level floor
(`:218-219`: "all 51 JSON rows maintain A/GO strict same-plane, throughput cells within
±1.0% of `SK-V17-open`, across every behavior wave"), per-wave maintain rows in §2
(`SPEC.md:297-299`: each of W1/W2/W3 carries "JSON 51/51 maintain"), and each wave exit
gate re-asserts ±1.0% (`:474, 570, 648, 716, 762`). P3-C gates it per-wave; the re-entry
signature (a JSON row dropping below its locked floor — twitter 17685, citm_catalog 28630,
`REDRESS.md:2828`) is named as the falsifier (`p3e:247-253`). The ±1.0% JSON floor is
cleanly distinguished from the -2.0% W2 CSS-typed recognition-plane maintain band
(`SPEC.md:564-568`, `p3e:252-253`). No reader will mistake one for the other. ACCEPT.

## §4 — PRUNE-before-rebuild order — ACCEPT

W1 is the PRUNE wave (DELETE fact-stream + W5C, THEN route into the existing tape; no
parallel path — `SPEC.md:392-394`). W2 is the rebuild (the full rich projection generator
generalizing W1's minimal cursor — `SPEC.md:494` family). W3 (NEON) is gated behind W1
tape activation (no structural index to scan into until the tape decodes CSS — `p3b:64,434`
"Substrate (W1 PRUNE/tape, W2 projection) strictly precedes NEON (W3)"). P3-B states the
split explicitly: "the V1 W1 (tape+projection merged) is split into SPEC W1 (PRUNE/tape) +
SPEC W2 (layout-driven projection)" (`p3b:38`). Topological order (telemetry → prune →
rebuild → NEON → L9 → close) holds. No wave rebuilds on an un-pruned base. ACCEPT.

## §5 — No wave silently re-opens a pre-blocked route — ACCEPT

Grep of `skinny/crates/` at HEAD `f87ee713a` for
`struct StructLayout|struct TapeCursor|struct UnionTape|TapeStructBuilder|struct StructRegistry`
returns ZERO — the Lock-1 no-second-substrate guard is honored at the current base, and the
SPEC forbids introducing them (SPEC §9 global block `:807-811`; per-wave W2 `:825`). No SPEC
wave's owner paths or tasks introduce a renamed scanner, a parallel substrate, a sidecar
producer, or a retained cursor. The cohort speaks ONE wave numbering (P3-A/P3-B/P3-E/SPEC
all W0–W5, L2/L7/L3-min in W1, L3-full/L8/L4 in W2, L1/L5/L6 in W3, L9 in W4, close in W5).
The 96/97/98 union-substrate retirement is re-admitted by NO wave; the tape route rides the
orthogonal REDRESS-140 differential (cardinality one, index == tape offsets), which the
ledger correctly distinguishes from the retired thesis (`p3e:51-62,318`). ACCEPT.

## §6 — Inherited family list — ACCEPT

SPEC §9 (`:830-832`) carries the full inherited family set: `28+33, 50-55, 60-72, 80,
82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV closed-enum production migration`
— the §3 CH3 mandate's `28+33, 50-55, 60-72, 80, 82-84, 88, 89, plus the historical blocked
routes` is fully satisfied AND extended with the SK-V14/15 retirements (96-98, 183/184/209-213,
215, 242-247, FNV). P3-E §2 "Inherited by ALL waves" (`p3e:198-239`) carries the semantics
of each family verbatim, not just the ids. ACCEPT.

## §7 — Folding directives for V3→V4

NONE. All V1 and V2 CH3 dispositions are folded. No CH3 defect — substantive or cosmetic —
remains:
- the pre-block ledger is complete and wave-correct (every per-wave pre-block named, cited,
  and SPEC-aligned);
- no wave re-opens a pre-blocked route (second-substrate grep ZERO; no renamed scanner);
- PRUNE-before-rebuild holds (W1 prunes, W2 rebuilds, NEON gated behind tape);
- the fact-stream/W5C retirement strands no consumer (every site named + tree-verified at
  HEAD + same-commit migrate/delete obligation + greppable exit gate);
- JSON 51/51 is guarded ±1.0% across every wave, distinguished from the -2.0% CSS-typed band;
- the inherited family list is full;
- the V2 citation-drift residual is resolved.

CH3 returns full ACCEPT on cycle V3.

## §8 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH3, §8.5 same-wave-consumer.
- `restart/skinny/tranches/sk-v17/SPEC.md`: §-anchors verified (`:337,390,494,588,668,730,782`);
  §1 JSON guard (`:42-44,218-219`); §2 manifest + per-wave maintain (`:264-269,297-301`); §2.1
  generality (`:305-335`); W0 no-generated-change (`:375`); W1 PRUNE owner paths + full
  consumer enumeration + same-wave gate (`:390-489`, esp. `:403-424,453-459,467-471`); W2
  projection + byte-equal + L8 + -2.0% band (`:494-583`, esp. `:534-536,550-556,564-568`);
  W3 NEON (`:588-666`); W4 L9 conditional (`:668-723`); W5 close + `regen --check` 9/9 +
  paper-close family (`:730-779`, esp. `:748,757,771-774`); §9 route ledger + per-wave
  attributions + inherited families (`:782-854`, esp. `:807-811,820-832,852-854`).
- `restart/skinny/tranches/sk-v17/research/p3/p3e-preblocked-ledger.md`: §1 mapping
  (`:64-80`), W0 (`:89-102`), W1 (`:104-127`), W2 (`:129-146`), W3 (`:148-162`), W4
  (`:164-175`), W5 (`:177-196`), inherited families (`:198-239`), §3 falsifiability
  (`:241-282`), §4 barred set (`:284-318`). Grep for stale `669/687/696/710-713/669-719`
  W5 citations returns NONE (V2 residual resolved).
- `restart/skinny/tranches/sk-v17/research/p3/p3b-wave-sequencing.md`: 6-wave reconciliation
  (`:31,38`), topological constraint (`:64,429,434`), wave manifest (`:144,150`).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V1/CH3.md`,
  `restart/skinny/tranches/sk-v17/research/p3/hardening/V2/CH3.md` (dispositions folded).
- `skinny/REDRESS.md`: 96/97/98 retirement + floors twitter 17685 / citm_catalog 28630
  (`:2828`), 140 SK-V16 W9 differential verified verbatim (`:4245-4252`).
- `skinny/crates/codegen/src/lib.rs` consumers verified at HEAD: `:299,336,567,569,581,597,
  611,613,1001,1035,1109,1113` (grep).
- `skinny/crates/runtime/src/lib.rs` consumers verified: `:76,91,108,126,143,162,434` (grep).
- `skinny/crates/runtime/src/grammars/css_l4_*/parser.rs:6` × 7 dirs (grep).
- `skinny/crates/codegen/src/runtime_generator.rs:621,666,694` (grep).
- Owner-path grep: `skinny/crates/` clean of `struct StructLayout|struct TapeCursor|
  struct UnionTape|TapeStructBuilder|struct StructRegistry` at HEAD `f87ee713a` (ZERO).
