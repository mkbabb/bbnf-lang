# SK-V17 P3 CHALLENGE — CH3 REGRESSION (V1)

Lens: CH3 REGRESSION. Cycle: V1. Date: 2026-05-29.
Pass: S-P3 Synthesis-Plan CHALLENGE (PASS-3 §3 CH3, ORCHESTRATOR §3W).
Master HEAD: `f87ee713a`.
Subjects reviewed: `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md`,
`restart/skinny/tranches/sk-v17/SPEC.md`.
Focus (per dispatch): no wave re-opens a REDRESS pre-block; PRUNE-before-rebuild
order honored; the fact-stream / W5C retirement does not strand a consumer;
JSON 51/51 guarded; the P3-E ledger correctly enumerates every per-wave pre-block;
the SPEC carries the full inherited family list.

## §0 — Verdict summary

| Wave / section | Disposition | Defect class |
|---|---|---|
| Inherited family list (SPEC §9 `:769-771`) | ACCEPT | — |
| Second-substrate guard (SPEC §1 Lock 1) | ACCEPT | grep-clean at HEAD |
| W0 (baseline) | REVISE | dirty-regen pre-block mis-attributed to W0 |
| W1 (PRUNE / tape) | REVISE | W5C/`emit_fact_stream` test consumers under-enumerated |
| W2 (projection) | REVISE | L8 pre-block wave-number desync (P3-E W1 vs SPEC W2) |
| W3 (NEON) | ACCEPT | — |
| W4 (L9, conditional) | ACCEPT | — |
| W5 (close) | REJECT | P3-E has NO close-wave pre-block row (5-wave ledger vs 6-wave SPEC) |
| PRUNE-before-rebuild order | ACCEPT | W1 prunes, W2 rebuilds |
| JSON 51/51 guard | ACCEPT | per-wave maintain present; one citation-looseness note |

Counts: ACCEPT 6 · REVISE 3 · REJECT 1.

## §1 — The load-bearing finding: P3-E is a 5-wave ledger keyed against a 6-wave SPEC

The single greatest CH3 defect is a **wave-numbering desynchronisation** between the
pre-block ledger (P3-E) and the authoritative wave plan (SPEC). The SPEC is the
contract (`SPEC.md:9`, `:256-264`): **six waves W0–W5**, with the candidate placement

- W0 = baseline (no L)
- W1 = PRUNE: L2, L7, L3-minimal (`SPEC.md:388-391`)
- W2 = projection generator: L3-full, **L8**, L4 (`SPEC.md:469-471`)
- W3 = NEON: L1, L5, L6 (`SPEC.md:539-541`)
- W4 = L9 commit-by-construction, conditional (`SPEC.md:624`)
- W5 = close / clean regen / Lock-14 audit (`SPEC.md:669`)

P3-E’s own wave→candidate mapping (`p3e-preblocked-ledger.md:67-73`) is a **five-wave
W0–W4** scheme with a different placement:

- W1 = tape: L2, L7, **L8** (`:70`)
- W2 = projection: L3, L4 (`:71`)
- W3 = NEON: L1, L5, L6 (`:72`)
- W4 = L9 (`:73`)
- (no W5 close row)

P3-B (`p3b-wave-sequencing.md:78-83`) is a THIRD scheme (W0–W4, W2 = NEON with L1/L4/L5/L6/L7).
Three S-P3 artefacts carry three different candidate placements. The SPEC is the binding
one; the ledger and sequencing artefacts must conform to it.

Consequences for CH3 specifically (the pre-block attribution must track the wave it guards):

1. **L8 sparse-flag pre-block is keyed to the wrong wave.** P3-E attributes the
   "Sparse-flag → sidecar/dense column" pre-block (REDRESS 96 class column; SK-V9 W9
   anti-sidecar `:6407-6411`) to **W1** (`p3e-preblocked-ledger.md:102`, in the
   "W1 — Tape activation" table). In the SPEC, **L8 lands in W2** (`SPEC.md:470,503,522`).
   A W2 triumvirate reading P3-E for "what W2 must not re-open" finds the L8 anti-sidecar
   pre-block filed under W1, not W2. The pre-block content is correct; its wave key is wrong.
   The SPEC §9 W2 row (`SPEC.md:764`) does carry it correctly ("L8 flag as hand-curated
   per-rule catalogue; … retained cursor / aux density / sidecar event vector"), so the
   SPEC is internally right — only P3-E is desynced.

2. **No W5 close-wave pre-block row exists in P3-E.** See §6 (REJECT).

3. **The dirty-generated close claim is mis-attributed to W0.** P3-E files
   "Dirty-generated close claim … `regen --check` 9/9 … `dirty_generated_state != clean`"
   under **W0** (`p3e-preblocked-ledger.md:89`). The SPEC's clean-regen 9/9 gate is the
   **W5 close** exit gate (`SPEC.md:687,696`), not W0 (W0 is "0 behavior LOC … NO
   parser/scanner/SIMD/codegen behavior or generated parser output change lands",
   `SPEC.md:367`). W0 must not regenerate; the dirty-regen pre-block belongs to W5.

**Disposition: REVISE (W0, W1, W2) + REJECT (W5).** Concrete fix: re-key the P3-E §1
mapping table and §2 per-wave sections to the SPEC's W0–W5 numbering; move the L8
anti-sidecar pre-block from the W1 table to a new W2 table; move the dirty-generated
close pre-block from W0 to a new W5 table; add the missing W5 close-wave pre-block row
(§6). The ledger's pre-block *content* is sound throughout — this is a keying defect, not
a content defect, which is why these are REVISE/REJECT and not a wholesale rebuild.

## §2 — Per-wave pre-block disposition

### W0 — baseline / telemetry — REVISE

The W0 pre-blocks correctly enumerate the broadcast-215 re-projection
(`p3e:86`; SPEC §3 `:372`), the fact-stream comparator re-wire (`p3e:87`; SPEC §3 `:350`,
SK-V15 W6 item 248), and warm/single-sample telemetry (`p3e:88`). These match the
SYNTHESIS §0.4 215 pre-block and the SK-V15 W6 `W6_SAMPLE_COUNT=1` retirement. ✓

Defect: the **dirty-generated close claim** pre-block is filed under W0 (`p3e:89`) but
the SPEC keys the `regen --check` 9/9 gate to **W5** (`SPEC.md:687,696`); W0 lands no
generated change (`SPEC.md:367`). **Fix:** move that pre-block row to a W5 close table.

### W1 — PRUNE: retire fact-stream + W5C → tape activation — REVISE

Pre-block enumeration is strong and correctly cited: union-substrate 96/97/98 RETIRED
(`p3e:97`, REDRESS `:2795-2950`, admissible only via the REDRESS-140 differential
`:4245-4252` — "Substrate cardinality remains one", verified at `REDRESS.md:4248-4251`);
AZ-IV eager (`p3e:98`); StructRegistry indirection (`p3e:99`); W5C relocated-not-retired
(`p3e:100`); fact-stream as admission plane (`p3e:101`); sparse-flag→sidecar (`p3e:102`,
but see §1 — this is a W2 concern under the SPEC); `split_off`/`Vec<Vec>` (`p3e:103`).
No re-opened route. ✓

**PRUNE-before-rebuild: ACCEPT.** W1 is explicitly the PRUNE wave — "It DELETES the
fact-stream String plane and the hand-coded routing array, THEN routes CSS into the
existing tape — it does not add a parallel tape path beside the String" (`SPEC.md:384-386`).
The rebuild (projection generator) is W2 (`SPEC.md:466-468`). Order honored.

**Defect — the fact-stream / W5C retirement under-enumerates its stranded consumers.**
The CH3 dispatch flags "the fact-stream/W5C retirement does not strand a consumer" as a
focus. The SPEC W1 owner-path list names the deletion site and two consumers:
`lib.rs:336` (array), `:567,:611` (the two `for profile in W5C_REQUEST_FACT_PROFILES`
loops), `:299` (selected) (`SPEC.md:395-396`). But a grep of the actual tree shows the
retirement strands MORE consumers, all asserting the `emit_fact_stream` round-trip:

```
lib.rs:299   selected via W5C_REQUEST_FACT_PROFILES
lib.rs:567   for profile in W5C_REQUEST_FACT_PROFILES { … emit_fact_stream }
lib.rs:581   assert generated contains "emit_fact_stream"
lib.rs:597   w5c_css_request(...) → asserts W7_SAME_SUBSTRATE_UNION
lib.rs:611   for profile in W5C_REQUEST_FACT_PROFILES { … check_dir }
lib.rs:1001  assert generated contains "emit_fact_stream"
lib.rs:1035  assert generated contains "emit_fact_stream"
lib.rs:1109  w5c_css_request("css_l4_declaration_values", …)
lib.rs:1113  w5c_css_request(..., &changed)
```

Deleting `W5C_REQUEST_FACT_PROFILES` and retiring `emit_fact_stream` strands the test
consumers at `:581, :597, :1001, :1035, :1109, :1113` (compilation/assertion breakage),
not only `:567, :611`. Per the same-wave-consumer non-negotiable (`SPEC.md:242-243`) and
PASS-3 §8.5, these test consumers must be migrated-to-tape-assertion or deleted in the
SAME W1 commit, else W1 leaves dangling references — a silent strand. **Fix:** extend the
SPEC §4 W1 owner-path list (`SPEC.md:395-396`) to enumerate the full consumer set
(`lib.rs:581,597,1001,1035,1109,1113`) and add a W1 task line: "migrate or delete the
`W5C_REQUEST_FACT_PROFILES` / `emit_fact_stream` round-trip test consumers in the same
commit; no dangling `emit_fact_stream` assertion survives." This is REVISE, not REJECT —
the retirement intent and principal consumers (`:567,:611`) are named; the enumeration is
incomplete.

### W2 — layout-driven lazy projection generator — REVISE

Pre-block content correct: parser-local structural-mask cursor 51/53 (`p3e:118`,
REDRESS `:1334-1336`); fake-generated-template / static-centralization 213 (`p3e:119`,
REDRESS `:5276-5293`); eager projection / preserve-rich-ast (`p3e:120`); second substrate
via `StructLayout`/`TapeCursor` (`p3e:121`). The SPEC §5 W2 entry/pre-block rows
(`SPEC.md:489-491,522-525,764`) carry the L8-as-hand-curated-catalogue and
index-as-parallel-vector (REDRESS-53) blocks correctly.

Defect: P3-E does not present these under a wave numbered W2-in-the-SPEC-sense for L8 —
the L8 sparse-flag pre-block is in P3-E's W1 table (§1 above). The W2 table in P3-E
covers L3/L4 only and omits the L8 anti-sidecar/hand-curated-catalogue pre-block that the
SPEC places in W2. **Fix:** add the L8 sparse-flag→sidecar and L8-hand-curated-catalogue
pre-blocks to P3-E's W2 table (re-keyed from W1). REVISE.

### W3 — NEON structural index — ACCEPT

All NEON pre-blocks correctly enumerated and (by coincidence of numbering) aligned
between P3-E W3 and SPEC W3: PMULL on the hot path (REDRESS 88 `:2510-2540`, L5 uses the
`escape_mask_64` `overflowing_add` carry idiom not PMULL — `p3e:133`, `SPEC.md:577,600`);
CTZ as unconditional default (REDRESS 89 `:2542-2585`, L6 default = scalar running balance
— `p3e:134`, `SPEC.md:580,600`); lo6/`classify_tbl4` on CSS (`;{`→slot-59 `& 0x3f`
collision; eq-set fan instead — `p3e:135`, `SPEC.md:570,599`); orphan kernel / net-new
without checkasm + same-wave consumer (REDRESS 88/89, SK-V5 W5 `:1255-1267`, udot/i8mm §4
— `p3e:136`, `SPEC.md:601`); micro-kernel without same-row gate (REDRESS 80/82-84 — `p3e:137`).
No re-opened route. The barred §4 set (orphan udot, net-new i8mm, FNV/hex, asmjson FSM,
lo6-on-CSS, D6 second substrate) matches HARDENING-S-P2-V3 §4 and the LOCKED ineligibility.
ACCEPT.

### W4 — L9 commit-by-construction (conditional) — ACCEPT

L9 re-profile hard gate correctly enumerated (`p3e:148`, SPEC §7 `:616-622,786-789`): the
28.87%+2.45% recognition-control figures are NOT a measured rollback antecedent; L9
admits only if a post-W1 N≥50 typed-tape re-profile surfaces a top-N rollback/control
leaf, else not-dispatched (honest non-admission, not a deferral). `split_off`/`Vec<Vec>`
(`p3e:149`, SPEC `:625-626`) and value-discard (`p3e:150`) blocks present. Aligned by
number between P3-E W4 and SPEC W4. ACCEPT.

## §6 — W5 close wave: REJECT (the ledger does not enumerate the close-wave pre-blocks)

The CH3 contract (PASS-3 §3 CH3) requires the P3-E ledger to "correctly enumerate every
REDRESS route each wave must not re-open." The SPEC defines a **W5 close wave**
(`SPEC.md:669-719`) with explicit pre-blocks (`SPEC.md:710-713,767`):

- paper close ("wired"/"integrated" without a bench-row threshold);
- deleting legacy CSS generated/runtime shims before replacement proof lands;
- full-codegen close claims while dirty generated CSS files remain;
- brace-counter proof as CSS admission;
- dropping falsifier rows;
- corpus-average claim substituting for per-corpus medians;
- the clean-regen 9/9 (`dirty_generated_state==clean`) gate.

**P3-E has NO W5 close-wave row.** Its §1 mapping table (`p3e:67-73`) stops at W4=L9, and
its §2 per-wave sections (`p3e:82-156`) have W0, W1, W2, W3, W4 — no close wave. The
dirty-generated close pre-block is mis-filed under W0 (§1.3 above); the
legacy-shim-deletion-before-proof and corpus-average pre-blocks appear in the SPEC but are
absent from the P3-E per-wave ledger entirely.

The SPEC itself carries these correctly in §8/§9, so no wave actually re-opens a route —
this is a ledger-completeness defect in P3-E, not a SPEC defect. But per the CH3 contract
the *ledger* must enumerate them per wave. **Fix:** re-key P3-E to W0–W5 and add a W5
close-wave table enumerating: full-codegen-close-while-dirty (§0.4 last bullet; SK-V15
W7-W11 `DifferentFile("generated.rs")` `:6350-6354`); legacy-shim deletion before
replacement proof (§0.4); corpus-average substituting per-corpus medians (SYNTHESIS §0.5,
SPEC `:88,713`); paper-close (`no-deferrals`); brace-counter proof as CSS admission
(SK-V15 W6 exclusions, `p3e:48-49`). REJECT until the W5 row is present.

## §3 — JSON 51/51 guard — ACCEPT (one citation-looseness note)

The JSON guard is well-defended. SPEC §0.1 row 1 (`:42-45`), §0.5 floor (`:213-214`,
"within ±1.0% of `SK-V17-open`, across every behavior wave"), and the per-wave maintain
rows in §2 (`SPEC.md:289-291`) and §3-§8 exit gates (`:364,446,516,594,655,701`) all
re-assert JSON 51/51 A/GO strict same-plane. P3-C gates it per-wave (`p3c:154-159`) and as
the close tripwire (`p3c:161`). The pre-block re-entry signature (a JSON row dropping
below its locked floor — twitter 17685, citm_catalog 28630, verified at `REDRESS.md:2828,2833`)
is correctly named as the falsifier (`p3e:206-212`). ✓

Note (not a defect, fold into REVISE): P3-E §3 cites the maintain tripwire as
">2% drop vs pre-wave = fail" (`p3e:212`, REDRESS 89 `:2573-2579`), while the SPEC JSON
guard floor is the stricter ±1.0% (`SPEC.md:214`). The -2% is the REDRESS-89 per-row
maintain idiom (and P3-C applies -2.0% to the CSS `track1_full_parse` recognition plane,
not the JSON guard); the JSON guard itself is ±1.0% everywhere it is the subject. Align
P3-E §3 to cite ±1.0% for the JSON guard specifically, reserving -2.0% for the CSS
recognition-plane maintain, to avoid a reader treating the looser threshold as the JSON
floor.

## §4 — PRUNE-before-rebuild order — ACCEPT

W1 is the PRUNE wave (DELETE fact-stream + W5C, THEN route into the existing tape;
no parallel path — `SPEC.md:384-386`). W2 is the rebuild (the full rich projection
generator generalizing W1's minimal cursor — `SPEC.md:466-468`). W3 (NEON) is gated
behind W1 tape activation (no structural index to scan into until the tape decodes CSS —
`SPEC.md:536-537`, SYNTHESIS §0.1 NEON gate). The topological order (substrate before
NEON, prune before rebuild, telemetry before any speed claim) holds. No wave rebuilds on
an un-pruned base. ACCEPT.

## §5 — No wave silently re-opens a pre-blocked route — ACCEPT (with the W1 strand caveat)

Grep of `skinny/crates/` at HEAD `f87ee713a` for the forbidden second-substrate types
(`TapeStructBuilder`, `struct StructLayout`, `struct TapeCursor`, `struct UnionTape`)
returns ZERO — the Lock-1 no-second-substrate guard is honored at the current base, and
the SPEC forbids introducing them (`SPEC.md:225-226,455-456,524,594,746-750`). No SPEC
wave's owner paths or tasks introduce a renamed scanner, a parallel substrate, a sidecar
producer, or a retained cursor. The W1 consumer-strand (§2 W1) is a same-wave-consumer
*completeness* gap (dangling test references), not a re-opened route. ACCEPT.

## §7 — Folding directives for V2

REVISE/REJECT dispositions to fold into the V2 dispatch:

1. **[REJECT → W5 row]** Re-key the P3-E ledger to the SPEC's W0–W5 numbering and ADD a
   W5 close-wave pre-block table (paper close; legacy-shim deletion before replacement
   proof; full-codegen-close-while-dirty; corpus-average substituting per-corpus medians;
   brace-counter proof). `p3e-preblocked-ledger.md:67-73` + new §2 W5 subsection.

2. **[REVISE → W2 keying]** Move the L8 sparse-flag→sidecar and L8-hand-curated-catalogue
   pre-blocks from P3-E's W1 table (`:102`) to a W2 table, matching the SPEC's L8-in-W2
   placement (`SPEC.md:470`).

3. **[REVISE → W0 keying]** Move the dirty-generated close pre-block from P3-E W0 (`:89`)
   to the new W5 table; W0 lands no generated change (`SPEC.md:367`).

4. **[REVISE → W1 consumer enumeration]** Extend SPEC §4 W1 owner paths (`SPEC.md:395-396`)
   to name the full `W5C_REQUEST_FACT_PROFILES`/`emit_fact_stream` consumer set
   (`lib.rs:581,597,1001,1035,1109,1113`, not only `:567,611`) and add a W1 task: migrate
   or delete the round-trip test consumers same-commit; no dangling `emit_fact_stream`
   assertion survives.

5. **[REVISE → citation]** Align P3-E §3 (`:212`) to cite ±1.0% as the JSON guard floor
   (per SPEC `:214`), reserving -2.0% for the CSS recognition-plane maintain.

Also flag for the cohort (CH4/CH5 cross-ref, informational): P3-A/P3-B/P3-E/SPEC carry
THREE distinct candidate-to-wave placements (P3-B has L7 in W2-NEON and a 5-wave scheme;
P3-E has L8 in W1; the SPEC has L7 in W1 / L8 in W2 / 6 waves). The SPEC is binding; the
sequencing and shortlist artefacts must be reconciled to it in V2 so the whole cohort
speaks one wave numbering. This is the root cause of the CH3 keying defects.

## §8 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH3, §8.5 same-wave-consumer.
- `restart/skinny/tranches/sk-v17/SPEC.md` §0.1 (`:42-109`), §1 Lock 1 (`:225-226`),
  §2 manifest (`:256-264`), §3 W0 (`:329-380`), §4 W1 (`:382-464`), §5 W2 (`:466-532`),
  §6 W3 (`:534-612`), §7 W4 (`:614-667`), §8 W5 (`:669-719`), §9 ledger (`:721-793`).
- `restart/skinny/tranches/sk-v17/research/p3/p3e-preblocked-ledger.md` §1 mapping
  (`:67-73`), §2 per-wave (`:82-156`), §3 (`:200-232`), §4 (`:234-268`).
- `restart/skinny/tranches/sk-v17/research/p3/p3b-wave-sequencing.md` (`:78-83`).
- `restart/skinny/tranches/sk-v17/research/p3/p3c-falsifiability-gates.md` (`:154-161`).
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.4 (`:185-236`).
- `skinny/REDRESS.md`: 96-98 retirement (`:2910,:2934`), 140 differential (`:4245-4252`),
  JSON floors twitter 17685 / citm_catalog 28630 (`:2828,:2833`).
- `skinny/crates/codegen/src/lib.rs` W5C/`emit_fact_stream` consumers
  (`:299,:336,:567,:581,:597,:611,:1001,:1035,:1109,:1113`).
- `skinny/xtask/src/regen_css.rs` seven `RequestFacts` literals (`:45,63,81,99,117,135,153`).
- Owner-path grep: `skinny/crates/` clean of
  `TapeStructBuilder|struct StructLayout|struct TapeCursor|struct UnionTape` at HEAD.
