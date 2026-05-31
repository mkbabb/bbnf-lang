# αA — SK-V17 Results Extraction (Pass Alpha SK-V17→SK-V18, cycle V5)

Agent: alphaA (cycle V5). Date: 2026-05-31. SK-V17 close SHA: `f6a38445b`.
V3-audit SHA: `7dbe44c22`. Alpha-hardening re-verify HEAD: `318d9c046` (unchanged since V3/V4).
Scope (PASS-ALPHA §2 α-A): extract SK-V17 close + V3-audit ground truth — JSON>sonic VALID;
CSS>lightningcss 1.9–3.3× cold (lazy-vs-eager caveat); substrate validated; the
hand-written/forked/phantom/x86/old-bench overfit surfaces with path:line.

This is the GENERALIZATION cycle's results inventory: it records WHAT SK-V17 proved (the
>SOTA, measurement-valid) and WHAT it left hand-written/forked (the SK-V18 backtrack targets),
each with citation, so the goalset (αF) can be bound to ground truth, not narrative.

**V5 FOLD (resolves the V4 CHALLENGE dispositions against αA — ONE REVISE absorbed: the FOLD-1 crate-wide x86 census):**
- **R-1 (V4 CH3 + CH7 BLOCKING REVISE — the second x86 surface; ABSORBED here):** the V4 CHALLENGE
  carried a concrete REVISE on αA. CH7 §1 "alphaA (results extraction) — the one REVISE" (CH7:57-115)
  + CH3 "alphaA-results-extraction.md — REVISE" (CH3:216-225,316-342) found that αA's x86 census
  scoped the PRUNE to **`src/x86_64/` ONLY** (24 files / 847 LOC) and MISSED the **SECOND x86
  surface** that the V4 cohort's FOLD-1 (origin: CH5 V3 §C.5/§F.7) added: the vendored x86 ASM tree
  `skinny/crates/bbnf-simd/ext/x86/` (**3554 LOC** — `bbnf.asm` 485, `x86util.asm` 1036, `x86inc.asm`
  1978, `LICENSE-VENDOR` 55), the nasm-rs assembler driver `bbnf-simd/build.rs` (**102 LOC**), the
  `Cargo.toml` `build = "build.rs"` (`:8`) + `nasm-rs = "0.3"` (`:19`) build-dep, and the
  `src/lib.rs:247` `ext/x86/bbnf.asm` contract reference. αA was the **LONE cohort artefact** to retain
  the `src/`-scoped enumeration + close-gate; αC §6 FOLD-1, SYNTHESIS `:58-75`/`:246`/`:491`, and
  HANDOFF inv-3 all carried the crate-wide scope. A P1 keyed to αA's `find …/src/x86_64 -type f = 0`
  would PASS GREEN while ~3656 LOC of x86 ASM + an x86-assembler build driver survived. **FOLDED at V5:**
  §0 census x86 row, §3.2 x86 row, and §5/§6 close-gate now name BOTH surfaces and a crate-wide gate
  (`find …/src/x86_64 …/ext/x86 -type f = 0` AND `grep -riE 'avx|gfni|sve|x86|nasm' bbnf-simd/`
  neutral-only), mirroring αC §6 FOLD-1 / SYNTHESIS `:491` / HANDOFF inv-3. All FOLD-1 facts
  re-verified on disk at HEAD `318d9c046` (below). This corrects the prior V4-cycle αA assertion that
  it "carried ZERO V3 REVISE/REJECT across all seven lenses" — CH5 V3 §C.5/§F.7 carried a BLOCKING
  REVISE that touches αA's x86 census; αA had been the one artefact that did not absorb it.
- **All other αA sections ACCEPT (V4, carried forward):** §1 JSON, §2/2.1 CSS+caveat, §3.1 generator,
  §3.3 phantom-G two-axis, §3.4 NEON/checkasm, §4 substrate, §7 synthesis. CH7 §2 "alphaA (all other
  sections … ACCEPT)" (CH7:317); CH5 §1 "αA tally: ACCEPT ×3" (CH5:65,351); CH2 §1 "αA tally: ACCEPT
  ×3, REVISE ×0" (CH2:87,419); CH1 §αA "ACCEPT — every claim cited + disk-verified; honest H1 framing;
  checkasm 12 correct; no 'x86 gone' close-claim (not a C.5 owner)" (CH1:64,350). **αA §3.4 remains the
  MODEL checkasm-count framing** — it AUTHORS the disk-true 12 single-kernel + 2 = 14 correction and
  names the false-`18` anti-pattern (CH7:72; CH1:83,167).
- The V3 cohort's lone V3 REVISE lived on **alphaD §1** (stale "18 differential harnesses" /
  `tests/checkasm_*.rs (18)` at `alphaD:85`) + the mirrored CH4 §6 stale "18" — both contradicting
  the disk-true 12 single-kernel + 2 = 14 that **αA §3.4 itself authored** (CH1:279, CH7:311). αA is
  the source-of-truth the checkasm REVISE pointed downstream consumers BACK to; on the checkasm axis
  nothing folds INTO αA. The x86 R-1 above is the one disposition that DID fold into αA at V5.
- All αA load-bearing facts RE-VERIFIED at HEAD `318d9c046` (unchanged since V3/V4). The x86 census
  now spans BOTH surfaces (V5 R-1 fold): (1) `src/x86_64/` = 24 files (23 `.rs`/742 LOC + 1 `.asm`/105
  LOC, 14 `unimplemented!`); (2) `ext/x86/` = 4 files / 3554 LOC (`bbnf.asm` 485, `x86util.asm` 1036,
  `x86inc.asm` 1978, `LICENSE-VENDOR` 55) + `build.rs` 102 LOC (nasm-rs driver) + `Cargo.toml`
  `build="build.rs"`:8 / `nasm-rs="0.3"`:19 + `lib.rs:247` `ext/x86/bbnf.asm` ref — all live-verified at
  HEAD (`find …/ext/x86 -type f|xargs wc -l|tail -1`=3554; `wc -l build.rs`=102). Other facts: 7 CSS
  `generated.rs` single md5; `CSS_GENERATED_RS` at `runtime_generator.rs:701`; `RuntimeEmitterKind` fork
  at `grammar_provider.rs:40`; `parse_w11_1_number` ×7 in `json/generated.rs`; `ValueRef<…,G:
  EventGrammar = AnyGrammar>` at `tape/mod.rs:175`; checkasm `*.rs` = 14. The carry is verbatim-correct.
- The **three V2 CH1 non-blocking notes** folded at V3 (x86 LOC dual-figure, working-tree caveat,
  "no V1 CONSOLIDATED") remain folded and re-verified below — carried forward unchanged:
  - **CH1 Note-1 (x86 LOC framing):** for the FIRST surface, "742 LOC" is the **`.rs`-only** count; the
    `.asm` (`byte_class_from_eq_set_64.asm`) is a separate **105 LOC**; the all-24-files total is
    **847**. The prune close gate is **content/file-count, not a LOC-budget** — and as of the V5 R-1 fold
    it is **crate-wide**, NOT `src/`-scoped: `find …/src/x86_64 -type f`=0 AND `find …/ext/x86 -type f`=0
    AND `build.rs` gone-or-aarch64-neutral AND `Cargo.toml` carries no `build=`/`nasm-rs` AND `grep -riE
    'avx|gfni|sve|x86|nasm' bbnf-simd/` neutral-only — so it deletes the entire x86 surface (847 + 3554 +
    102 LOC ≈ 4500) regardless of LOC framing. §3.2 + §0 + §5/§6 now state both surfaces explicitly
    (re-verified at HEAD: `src/x86_64/` 23 `.rs`=742 LOC + 1 `.asm`=105 LOC, 14 `unimplemented!`;
    `ext/x86/` 3554 LOC; `build.rs` 102 LOC).
  - **CH1 Note-2 (working-tree caveat stale):** the §6 caveat warned of a working-tree `diff`
    DIFFERS that no longer exists — the 7 CSS `generated.rs` have been re-regenerated to parity,
    so the **working-tree md5 now collapses to a single hash** (`md5 …css_l4_*/generated.rs |
    sort -u | wc -l` = **1** at HEAD `318d9c046`). The caveat is rewritten in §6 to state the
    current truth (working-tree AND `f6a38445b` both byte-identical, single md5) rather than warn
    of a vanished refutation; the load-bearing replica claim (7 identical at `f6a38445b`) is
    re-verified TRUE (`git show f6a38445b:… | md5 | sort -u | wc -l` = **1**).
  - **CH1 Note-3 ("no V1 CONSOLIDATED"):** this stale statement lived in αC/αD, **NOT αA** — αA
    never asserted it, so there is nothing to fold here. Recorded for completeness only.

(All prior V2 folds — CH7 x86 24-file, CH5 two-axis `K`/`G`, CH4 checkasm-12, CH2 Sheets-source —
were ACCEPTed at V2 and are carried forward verbatim below, each re-verified at HEAD `318d9c046`.)

---

## §0 — Headline (the inflection ground truth)

SK-V17 closed at `f6a38445b` with **both grammars >SOTA, measurement-valid, on a unified
substrate — but on HAND-WRITTEN, FORKED parsers.** This is the inflection point the campaign
named. The numbers are real; the implementation is not yet grammar-derived. SK-V18 is the
backtrack.

| axis | ground truth | status |
|---|---|---|
| JSON > sonic-rs strict | 51/51 admitted rows, cold per-parse, strict per-iter equality; Track 1 > sonic-strict **+1.4% … +164.7%** | **VALID** |
| CSS > lightningcss | rich-typed Track1 / lightningcss full-CSSOM = **1.996× … 3.348×**, N=200 cold median, real corpus | **VALID (lazy-vs-eager caveat — H1)** |
| Substrate union (Lock 1) | one `Tape`/`ValueRef`/`PayloadArena`; both grammars ride it; no second tape | **VALIDATED (the genuine foundation)** |
| Generator grammar-driven | DOES NOT EXIST — two forked hand-written parsers (const-`&str` CSS + string-literal JSON) | **OVERFIT (SK-V18 backtrack)** |
| ValueRef `<G>` parametricity | PHANTOM on the `G` axis — `G` never instantiated with a real grammar (always `AnyGrammar`); the `K` axis IS real for JSON | **DECORATIVE on `G` (instantiate-or-delete the `G` axis)** |
| aarch64-only | VIOLATED — x86 present on TWO surfaces (V5 R-1): (1) `src/x86_64/` **24 files (23 `.rs` = 742 LOC + 1 `.asm` = 105 LOC; 847 total), 14 `unimplemented!`**; (2) `ext/x86/` vendored ASM **3554 LOC** + nasm `build.rs` **102 LOC** + `Cargo.toml` nasm-rs dep + `lib.rs:247` ref (≈ −4500 LOC total) | **OVERFIT (P1 delete BOTH; close-gate crate-wide: `find …/src/x86_64 …/ext/x86 -type f`=0 AND `grep -riE 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` neutral-only)** |

---

## §1 — JSON > sonic-rs strict (VALID; 51/51 admitted rows)

Source: `skinny/RESULTS.md` (155 lines; 51 admitted JSON rows = 17 corpora × 3 workloads
{parse_only, direct_to_struct, real_typed_struct}; all `A`/`GO`/`strict`). The `parse_only`
row is the unconditional proof; `direct_to_struct`/`real_typed_struct` are conditional on the
per-corpus typed-struct schema. Per-iter equality PASS vs sonic_rs/serde; cold per-parse;
no broadcast (each row is a distinct measured median). Comparator: **sonic-rs strict** (rebuilt
without `utf8_lossy` — the SK-V6 plane; the strict skipper at `sonic_skipper.rs:5-6` uses
`IgnoredAny::deserialize` + `.end()`). `Δ vs sonic-strict` is column 22 of the RESULTS schema.

**Per-corpus Track 1 Mbps vs sonic-strict Mbps (Δ vs sonic-strict):**

| corpus | parse_only T1 / sonic / Δ | direct_to_struct T1 / sonic / Δ | real_typed_struct T1 / sonic / Δ |
|---|---|---|---|
| twitter | 8349.3 / 4913.1 / **+69.9%** | 17585.7 / 14857.6 / +18.4% | 10705.1 / 8952.3 / +19.6% |
| citm_catalog | 9079.8 / 8335.8 / +8.9% | 33366.5 / 21250.0 / **+57.0%** | 20512.6 / 12662.3 / **+62.0%** |
| canada | 16709.9 / 12970.9 / +28.8% | 4749.6 / 2733.7 / **+73.7%** | 4761.9 / 2736.4 / **+74.0%** |
| apache_builds | 13129.3 / 12951.7 / +1.4% (thinnest) | 7483.8 / 6327.8 / +18.3% | 4352.3 / 3390.8 / +28.4% |
| github_events | 8148.6 / 5014.4 / **+62.5%** | 12501.5 / 11012.9 / +13.5% | 6643.7 / 5975.2 / +11.2% |
| update_center | 5671.3 / 4707.6 / +20.5% | 12820.2 / 10887.3 / +17.8% | 6776.3 / 5845.4 / +15.9% |
| mesh | 11669.3 / 6589.8 / **+77.1%** | 9036.4 / 7875.3 / +14.7% | 4580.3 / 4343.2 / +5.5% |
| random | 3093.7 / 2937.3 / +5.3% | 7977.9 / 5754.7 / +38.6% | 4354.3 / 3041.0 / +43.2% |
| gsoc-2018 | 13213.3 / 11355.4 / +16.4% | 7228.2 / 6669.7 / +8.4% | 7176.7 / 6627.7 / +8.3% |
| marine_ik | 9505.5 / 5338.9 / **+78.0%** | 11162.2 / 8830.4 / +26.4% | 5515.1 / 5241.0 / +5.2% |
| instruments | 4281.8 / 3457.3 / +23.8% | 18191.8 / 14488.5 / +25.6% | 9550.1 / 7779.1 / +22.8% |
| numbers | 14472.3 / 7452.8 / **+94.2%** | 12574.7 / 11309.3 / +11.2% | 6608.6 / 6022.9 / +9.7% |
| unicode_mixed | 7379.3 / 7011.3 / +5.2% | 5903.6 / 5340.2 / +10.5% | 5837.9 / 5309.6 / +9.9% |
| unicode_escapes | 7897.4 / 2984.1 / **+164.7%** (widest) | 2357.5 / 1852.5 / +27.3% | 2244.5 / 2036.7 / +10.2% |
| unicode_basic | 9445.7 / 7059.9 / +33.8% | 6177.3 / 4692.7 / +31.6% | 3221.3 / 2480.5 / +29.9% |
| distinct_values | 5155.2 / 3233.8 / +59.4% | 8755.2 / 3907.3 / **+124.1%** | 8827.5 / 3895.1 / **+126.6%** |
| y_string_unicode | 3169.9 / 2417.9 / +31.1% | 5493.5 / 4263.6 / +28.8% | 5361.6 / 4266.9 / +25.6% |

- **Range of Δ vs sonic-strict: +1.4% (apache_builds/parse_only, the thinnest) … +164.7%
  (unicode_escapes/parse_only, the widest).** Every one of the 51 rows is positive — no JSON
  row is at or below sonic-strict. This is the SK-V18 regression tripwire: any grammar-DERIVED
  JSON parser (G1/G5) that drops a row below its sonic-strict comparator has lost the >SOTA.
  (αA is the canonical source of this range; the V1 SYNTHESIS/HANDOFF "+1.4%–78%" understatement
  was a downstream echo error fixed in those artefacts at V2, not here.)
- Hot leaf per row: `profile_direct-cold` TSV cited per row; the RESULTS schema records
  `hot-leaf=not-collected-in-W*-row` for the SK-V14 rows (a telemetry gap carried, not a
  defect). simdjson DOM / yyjson columns are populated only on `canada/parse_only` (13932 lossy,
  11493 DOM, 13003 yyjson — Δ vs simdjson DOM +45.4%, Δ vs yyjson +28.5%); the other rows carry
  `n/a` because those comparators were not run per-row.
- **Other JSON ground truth (V3 audit):** FNV closed-enum is **bench-only, never migrated to
  runtime** (quarantine holds, A1/A5). serde_json strict baseline is column 21 (Track 1 beats it
  on every row).

---

## §2 — CSS > lightningcss (VALID; 1.996×–3.348× cold; lazy-vs-eager caveat)

**The CSS >SOTA numbers DO NOT live in `skinny/RESULTS.md`.** RESULTS.md holds 24 `css_l4/*`
rows (lines 112–135), all `not_admitted:SK-V15-W0-broadcast-diagnostic` / `AUDIT-FALSIFIED` —
the retired 24-row broadcast (one timing tuple `track1=2319.041 / cssparser=2362.037 /
lightningcss=929.281` projected across 24 conceptual rows; a pre-blocked route, NOT the
headline). **ZERO admitted typed CSS rows exist in RESULTS.md.**

The real >SOTA is in the SK-V17 W5 close ledger
(`restart/skinny/tranches/sk-v17/research/w5/skv17-W5-close-ledger.md` §3, §7, the N=200 medians
at `:99-102`), measured by the canonical harness `w2_rich_cssom_bench` / `css_canon_bench` (cold,
real corpus 71KB–495KB, N=200 median, distinct per-corpus medians, no broadcast, independent
9-field cssparser oracle):

| corpus | class | rich-typed Track1 | lightningcss full-CSSOM | **rich/lcss** | W0 lcss bar | crosses? |
|---|---|---:|---:|---:|---:|---|
| bootstrap | **regular** | 2473.1 | 1119.1 | **2.210×** | 1112.4 | YES |
| animate | **regular** | 2937.9 | 1247.7 | **2.355×** | 1218.7 | YES |
| tailwindcss | nested/utility | 2773.4 | 828.5 | **3.348×** | 841.3 | YES (honest) |
| material-components-web | irregular | 2618.5 | 1312.0 | **1.996×** | 1292.3 | YES |

- **Range: 1.996× (material) … 3.348× (tailwind); both regular corpora cross (bootstrap 2.210×,
  animate 2.355×).** This matches the contract's "1.9–3.3×" framing. The lightningcss re-baseline
  (1119/1248/828/1312) matches the W0-LOCKED bar (1112/1219/841/1292) within ~5% — same-run,
  W0-comparable.
- EXACT **9-field** cssparser structural equality re-proven (the rich plane); preserve-rich-ast
  intact; JSON 51/51 held. A5 live-reproduced the ratios (2.15/2.91/1.91/1.98×) without
  `target-cpu=native` dependence — measurement-VALID. (αB §2.2 headlines the N=200 medians as
  Plane A "1.996–3.348×" and the live N=80 ratios as Plane B "1.9–2.9×" cross-check; the
  provenance difference is disclosed and the planes are non-mixable, not a contradiction.)

### §2.1 — The lazy-vs-eager caveat (V3 C2 — MEDIUM; binds H1)

The CSS >SOTA is **measurement-valid but NOT equal-work.** `track1_rich` *counts* 9 aggregate
fields **lazily** (zero payload writes, value-head classification) while lightningcss *builds an
owned typed CSSOM*. The rich rider costs ~25–33% over the 4-field path (proving real per-node
work — "materially less severe than a brace-counter"), but the honest framing is **"lazy
rich-summary beats eager full-CSSOM,"** NOT "equal-work CSSOM beats CSSOM." SK-V18 H1: re-frame
the claim OR add a symmetric materialization-depth comparator. The canonical `css_canon_bench` is
the honest harness — KEEP it. (The OLD contrived path is deleted by P2 — see §3.2.)

---

## §3 — Overfit surfaces (the SK-V18 backtrack targets), with path:line

### §3.1 — The "grammar-driven generator" does not exist (V3 D1, HIGH)

| surface | path:line | finding |
|---|---|---|
| CSS const-`&str` scanner | `skinny/crates/codegen/src/runtime_generator.rs:701` (`const CSS_GENERATED_RS: &str = r#"…"#`, raw-string body runs `701→1611`, ~910 LOC; `runtime_generator.rs` total 1611 LOC) | **verbatim-blob**: a `@generated` header over a hand-written `&str` literal. The `.bbnf` grammar is never consumed by the CSS emit path (`emit_request_facts` feeds only config constants). Identical SK-V16 finding, UN-REMEDIATED. **G2 backtrack target.** |
| JSON string-literal templates | `runtime_generator.rs:195` (`JSON_PARSE_ONLY_GENERATED_RS`), `:550` (`JSON_PARSE_ONLY_PARSER_RS`), `:572` (`JSON_MOD_RS`), `:594` (`JSON_HOST_RS`); CSS siblings `:598` (`CSS_MOD_RS`), `:612` (`CSS_PARSER_RS`), `:665` (`CSS_SINK_RS`) | JSON hot parser = fixed Rust string literals; grammar only `validate()`-gates emission, does not shape it. `json_sink_direct::render` (`json_sink_direct.rs:4`, `pub fn render(program: &SinkOnlyProgram)`) is the JSON inflection wave. **G1 backtrack target.** |
| FORKED generator | `skinny/crates/codegen/src/grammar_provider.rs:40` (`enum RuntimeEmitterKind { CompiledLowering, RequestFacts }`); branched at `:110` (`if request.profile_contract.emitter != RuntimeEmitterKind::RequestFacts`) | **single-emitter-path** violation: a grammar-family fork (JSON=`CompiledLowering`, CSS=`RequestFacts`) behind an abstract enum. **G3 backtrack target.** |
| 7 CSS replicas | `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` (7 dirs: at_rules_and_media, declaration_values, declaration_values_extended, nested_layout, stylesheet_selectors, vendor_and_custom_atrules, visual_functions) | **distinct-grammar-output** violation: all 7 are **byte-identical** (`md5 …css_l4_*/generated.rs \| sort -u \| wc -l` = **1**, verified at both `f6a38445b` and HEAD `318d9c046`). ONE CSS parser replicated 7× — overstates "7 grammars." All share `stylesheet.bbnf` / `entry_rule: stylesheet` (`regen_css.rs:23,41`). **P3 collapse target.** |

### §3.2 — Contrivance / wrong-arch surfaces

| surface | path:line | finding |
|---|---|---|
| **x86 surface — BOTH trees (V5 R-1 fold)** | **(1)** `skinny/crates/bbnf-simd/src/x86_64/` — **24 files = 23 `.rs` (742 LOC) + 1 `.asm` (`byte_class_from_eq_set_64.asm`, 105 LOC); 847 LOC total; 14 `unimplemented!`** stubs; declared unconditionally at `bbnf-simd/src/lib.rs:5` (`pub mod x86_64;`); referenced under `cfg(avx512bw)` at `lib.rs:285-287`. **(2) SECOND x86 surface** (FOLD-1, the V3 REVISE prior cycles missed): `skinny/crates/bbnf-simd/ext/x86/` vendored x264/FFmpeg ASM — `bbnf.asm` 485, `x86util.asm` 1036, `x86inc.asm` 1978, `LICENSE-VENDOR` 55 = **3554 LOC**; `bbnf-simd/build.rs` nasm-rs assembler driver **102 LOC** (`:38-40` non-x86 early-return, `:52` `ext/x86` include-root, `:56-76` `nasm_rs::Build … rustc-link-lib=static=bbnf_simd_asm`); `Cargo.toml` `build="build.rs"` (`:8`) + `nasm-rs="0.3"` (`:19`); `src/lib.rs:247` `ext/x86/bbnf.asm` contract ref | **aarch64-only VIOLATION** (V3 D3 + V4 FOLD-1 / CH5 V3 §C.5). 0 real x86 intrinsics in `src/x86_64/`; `ext/x86/` is dormant-on-aarch64 (`build.rs:38-40` early-returns on non-`x86_64`; no aarch64 consumer) — REVISE-not-REJECT, but it falsifies the literal "x86 gone" claim. ≈ **−4500 LOC** total. **P1 DELETE the ENTIRE x86 surface — close gate (crate-wide, NOT `src/`-scoped): `find …/src/x86_64 -type f`=0 AND `find …/ext/x86 -type f`=0 AND `build.rs` gone-or-aarch64-neutral AND `Cargo.toml` carries no `build=`/`nasm-rs` AND crate-wide `grep -riE 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` returns only aarch64-neutral comments.** (The gate is by content/grep, not LOC-budget; the LOC figures are accounting-honesty.) Verified at HEAD `318d9c046`: `src/x86_64/` 23 `.rs` + 1 `.asm` = 24 / 847 LOC / 14 `unimplemented!`; `ext/x86/` 4 files / 3554 LOC; `build.rs` 102 LOC; `Cargo.toml` `:8`,`:19`; `lib.rs:247`. |
| OLD contrived CSS bench | `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3091` (`fn measure_mbps`), `:528` (`fn lightningcss_facts` + per-grammar `*_lightningcss_facts` siblings) | **timed-plane-symmetry / corpus-in-timer** violation (V3 C3): warm (16+2000 iters), 85–357-byte SHA256-pinned micro-fixtures (not the real corpus), timed lightningcss does MORE work (parse + SHA256 + a second cssparser re-parse). Did NOT produce the headline numbers (those = `css_canon_bench`), but a live contrivance + confusion hazard. **P2 DELETE.** |
| metalang bench-id leak | `skinny/crates/runtime/src/grammars/json/generated.rs` — **7 occurrences** of `parse_w11_1_number` (a bench wave-id) baked into the SHIPPED JSON runtime (`grep -c` = 7 at HEAD) | regen-discipline violation (V3 misc). **P5 PURGE.** |
| Lock-14 gate exclusion holes | `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2409` (`const GENERIC_SCAN_ROOTS: &[&str]` omits `runtime_generator.rs` + template files; x86 tagged `"diagnostic-x86"`; consumed at `:2467,:2508,:4956`) | **A green gate over standing leaks.** `accepts_current_allowlist` PASSES (2/0) — NOT a known-failure; it passes by excluding the leak surface (V3 D4). **P4 FIX** — extend roots so the gate is meaningful. (Crate path pinned at V3: the gate lives in `bbnf-bench`, not `codegen`.) |

### §3.3 — Phantom generic + divergent value API (V3 D2, HIGH)

**`ValueRef` has TWO defaulted generic axes** (verified four-slot at `tape/mod.rs:175`, HEAD
`318d9c046`): `pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar>`.
The two axes are NOT the same phantom (carried from the V2 CH5 §A.1 fold):
- **`K` (Kind) is REAL for JSON** — instantiated with NumberKind/StringKind/ObjectKind/… in the
  JSON typed view; untyped-default (`AnyKind`) for CSS.
- **`G` (EventGrammar) is the PHANTOM** — only ever `AnyGrammar` in production; never instantiated
  with a real grammar; `EventGrammar`'s methods have zero non-test call sites.

**G4's instantiate-or-delete targets the `G` axis SPECIFICALLY.** Binding `K` to a real `Kind`
(already done for JSON) does NOT discharge the phantom; nor does deleting `G` while leaving the
impl coupling unchanged. The close condition must name WHICH axis (the `G` axis), or G4 produces
a false-green "instantiation" of the wrong axis.

| surface | path:line | finding |
|---|---|---|
| phantom `G` axis | `pub struct ValueRef<…, G: EventGrammar = AnyGrammar>` at `tape/mod.rs:175`; trait `EventGrammar` in `skinny/crates/runtime/src/tape/`; witnesses `…/json/event_grammar_witness.rs:4` (`struct JsonEventGrammar`, impl `:17`) + `…/sheets_witness/event_grammar_witness.rs:4` (`struct SheetsEventGrammar`, impl `:16`) | **phantom-generic** on the `G` axis: `G` never instantiated with a real grammar (always `AnyGrammar`); the witnesses are inert. The W2 "grammar-parametric projection" claim is NOT load-bearing on `G`. **G4: instantiate-or-delete the `G` axis** (DELETE is the abrogate-before-patch default). |
| divergent value API | JSON tree: `…/json/value.rs:144`, `…/json/view.rs:68` (`DocumentView` impl) / `:86…256` (`ValueRef<'doc,'input,Kind>` typed on `K`, `get(key)`, visitor). CSS flat stream: `…/css_l4_*/generated.rs:46` (`node: ValueRef<'doc,'input>` — both `K` and `G` defaulted, untyped, no visitor, not `DocumentView`) | The typed `ValueRef` (typed on the `K` axis: NumberKind/StringKind/ObjectKind/…) IS real for JSON; the phantom is the separate `G` axis. JSON + CSS share the `at_cursor` *pattern* hand-copied — **no shared Value/Document/Cursor trait.** **G4 target (shared trait + the `G`-axis decision).** |

### §3.4 — NEON wiring honesty (V3 C1, HIGH — corrects an SK-V17 W3 close claim)

The W3 NEON kernels are checkasm-validated, but the hot-path CSS scan is **scalar**:
- `find_css_significant` / `find_comment_close` callers in `skinny/crates/runtime/src/lib.rs:574,598,608` sit inside the **`#[cfg(test)]` "W3 NEON runtime-consumer parity guards"** module (header at `lib.rs:498-504`, scalar refs `significant_ref`/`comment_close_ref`/`count_top_level_commas_ref` at `:506,518,529`) — **dead at admission** (these are test guards, not the hot path).
- Only `count_top_level_commas` reaches a generated module, in the **cold** rich-summary (`nonjson_css_l4.rs:2945` via `rich_count_top_level_commas` `:3424`).
- **acceleration-wiring** violation: the W3 commit title ("NEON structural-index acceleration") overstates what is wired. **G6: wire-or-retire honestly.**
- ASM backlog (V3 C1/A4): 5 kernels wired as "neon" are scalar passthroughs — `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_class_from_table_64`, `eob_pad_clamp` (both `bbnf-simd/src/scalar/*.rs` and `aarch64/*.rs` present); the UDOT `digit_mac` is an orphan. PMULL/TBX/CSSC aarch64 backlog. The kernel BODIES that G6 rewrites live in the per-kernel files (`aarch64/bitmap_prefix_xor_64.rs:2`, `aarch64/eob_pad_clamp.rs:4`, …); the relabel/retire lands at the dispatch registration (`dispatch.rs:67-85`).
- **Checkasm baseline (carried from V2 CH4 §5 fold):** the disk-verified surface is **12 single-kernel
  differential harnesses + 2 harness/aggregate files** (`checkasm_common.rs` trampoline/canary +
  `checkasm_parity.rs` aggregate) = **14 `checkasm_*.rs` total** under
  `skinny/crates/bbnf-simd/tests/` — NOT "18." (Re-verified at HEAD `318d9c046`:
  `ls …/tests/checkasm_*.rs \| wc -l` = 14; the 12 single-kernel files are ascii_set_member_find_64,
  bitmap_next_set_bit, bitmap_prefix_xor_64, bracket_depth_mask_64, bulk_emit_positions_64,
  byte_class_from_eq_set_64, byte_class_from_table_64, comment_body_mask_64, eob_pad_clamp,
  escape_mask_64, structural_terminator_64, utf8_block.) Any S-P3 gate that asserts "18 checkasm
  harnesses present" would be un-satisfiable and would red-flag a clean tree — the exact P4-class
  false-gate anti-pattern this cycle is fixing. Correct framing: **KEPT and EXTENDED — current
  N=12 single-kernel differentials; each new G6 NEON body adds 1.** (NEON discipline is otherwise
  STRONG: real differential checkasm harnesses, scalar-ref-as-spec, grammar-neutral byte-set
  kernels.)

---

## §4 — Substrate validated (the genuine, generalizable foundation — Lock 1 holds)

V3 A6 VERIFIED: one `Tape`/`ValueRef`/`PayloadArena` in `skinny/crates/runtime/src/tape/`
(`mod.rs:94,175,38` + `assembler.rs:42,71`); both grammars ride it; CSS at-rule tag reuses the
sparse flag pair — **no second tape.** This is the load-bearing carry-forward: the SUBSTRATE
generalizes (it is the foundation the backtrack builds on); the value-API + codegen demonstrably
do NOT yet. SK-V18 backtracks the latter two over the (already-unified) former.

Pre-blocked (do NOT re-open; carried verbatim from the seed): AZ-IV eager value-tree;
StructRegistry per-leaf indirection; fact-stream-as-output; the 24-row broadcast; FNV production
migration (FNV stays bench-only); x86/AVX/SVE; brace-counter CSS admission; a second
substrate (`StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the landed `Tape`/`ValueRef`
is a Lock-1 violation — the projection generator emits accessors over the EXISTING types).

---

## §5 — Per-row close-condition seeds for αF (binding inventory)

What the αF goalset must preserve / move, sourced here:

1. **JSON 51/51 rows preserved** (§1): the regression tripwire for G1 (`json_sink_direct` projects
   from grammar) + G5 (JSON scanner onto neutral NEON). Floor = each row's `Δ vs sonic-strict`
   stays positive; thinnest margin apache_builds/parse_only +1.4% is the row most at risk.
2. **CSS 4 corpora preserved** (§2): bootstrap 2.210× / animate 2.355× / tailwind 3.348× /
   material 1.996× over lightningcss full-CSSOM, from the grammar-DERIVED CSS recognizer (G2).
   Both regular corpora must keep crossing. The αE preservation floor is pinned to the **N=200
   `css_canon_bench` per-row Mbps with a −3% floor** (bootstrap ≥2398.9 / animate ≥2850.0 /
   tailwind ≥2690.2 / material ≥2540.0), the N=80 set demoted to cross-check — never mix the
   planes. H1 re-frame binds the comparator honesty.
3. **PROVE Sheets — and where the `.bbnf` comes from** (carried from V2 CH2 §1.4 fold): the third
   grammar's source is **`grammar/google-sheets/google-sheets.bbnf`** (185 LOC), which EXISTS but
   in the **totality** tree, NOT the benched skinny tree. The benched skinny tree consumes only
   `skinny/grammars/json.bbnf` + `grammar/css/l4/stylesheet.bbnf` (via
   `skinny/xtask/src/{main.rs:172, regen_css.rs:16}`); there is **NO skinny Sheets source and NO
   skinny xtask Sheets `RuntimeTarget`** today. So the PROVE wave's first obligation is to bring
   the Sheets `.bbnf` into the benched tree (a skinny grammar root + an xtask Sheets target) — that
   is itself part of the litmus, not a given. The current `sheets_witness`
   (`…/sheets_witness/event_grammar_witness.rs`, a ~16–25-LOC inert `EventGrammar` impl) must become
   a real third grammar **via the generator ONLY** — its `generated.rs` must be non-identical to
   JSON's and CSS's (distinct-grammar-output).
4. **PRUNE close conditions**: P1 ENTIRE x86 surface gone — BOTH `src/x86_64/` (**all 24 files: 23 `.rs`
   + 1 `.asm`**) AND `ext/x86/` (3554 LOC vendored ASM) AND nasm `build.rs` (102 LOC) AND the `Cargo.toml`
   `build=`/`nasm-rs` dep AND the `lib.rs:247` ref; crate-wide close-gate `find …/src/x86_64 …/ext/x86
   -type f`=0 AND `grep -riE 'avx|gfni|sve|x86|nasm' bbnf-simd/` neutral-only (V5 R-1 fold — NOT the prior
   `src/`-scoped `find …/x86_64 -type f`=0, which would PASS GREEN over the surviving second surface);
   P2 old bench gone (`measure_mbps`/`*_lightningcss_facts` absent);
   P3 7 replicas → 1 CSS grammar (or N distinct); P4 `GENERIC_SCAN_ROOTS`
   (`bbnf-bench/src/lock14_baseline.rs:2409`) covers `runtime_generator.rs` + templates, gate
   red-or-meaningful; P5 `parse_w11_1_number` 0 occurrences in shipped json/generated.rs.
5. **GENERALIZE close conditions**: G3 `RuntimeEmitterKind` retired (one emitter path); G4 the
   phantom **`G` axis** of `ValueRef` instantiated-or-deleted (NOT the already-real `K` axis) +
   shared Value/Document/Cursor trait both grammars impl; G6 CSS NEON reached at admission (not
   `#[cfg(test)]`) or retired honestly. The G6 checkasm surface to KEEP/EXTEND is **12 single-kernel
   differentials** (§3.4), not 18 — any gate must assert against 12, not a phantom 18.

---

## §6 — Caveats / corrections (honesty, for downstream CH verification)

- **Audit ground truth is pinned to SHA `f6a38445b`** (the SK-V17 close + V3-audit base); all
  re-verification (V3 through V5 passes) ran at HEAD `318d9c046` and reproduced.
- **The 7 CSS `generated.rs` are byte-identical at BOTH `f6a38445b` AND HEAD `318d9c046`**
  (CH1 Note-2 fold — corrects the V2 stale caveat): the regen-noise divergence the V2 αA §6
  warned of has been re-regenerated to parity, so the **working-tree md5 now collapses to a
  single hash** (`md5 …css_l4_*/generated.rs | sort -u | wc -l` = **1**), AND the close-SHA md5
  is the same single hash (`git show f6a38445b:… | md5 | sort -u | wc -l` = **1**). V3 D1 (7
  replicas) holds as binding ground truth; there is NO working-tree divergence to caveat. CH-Regression
  can pin replica/diff-census claims to either SHA — both report identical.
- **NEON-wiring nuance**: the `find_css_significant`/`find_comment_close` call sites in
  `runtime/src/lib.rs:574,598,608` are within a `#[cfg(test)]` parity-guard module (module header
  `:498-504`), consistent with V3 C1 (dead at admission). Recorded so a grep that finds these
  strings is not mis-read as "wired into the hot path."
- **CSS numbers are NOT in RESULTS.md**: the 24 `css_l4/*` RESULTS rows are the FALSIFIED
  broadcast, not the >SOTA. The >SOTA lives in the W5 close ledger / `css_canon_bench`. αF's §4.3
  telemetry binding for SK-V18 must add per-corpus CSS rows to RESULTS.md (an SK-V18 RESULTS-plane
  fold the W5 ledger §3.1 already names).
- **simdjson/yyjson columns sparse**: only `canada/parse_only` carries them; the §4.2 strict
  comparator gate for SK-V18 should widen these per-row where the comparator is runnable.
  yyjson/asmjson/RapidJSON are NOT wired on aarch64 (only simd-json + sonic-rs in `Cargo.toml`) —
  their RESULTS columns are honest `None`, and the gate must NOT be read to require an un-run
  engine's number (a fabricated competitor column is REJECTed).
- **V5 FOLD ledger — R-1 (the V4 CH3 + CH7 BLOCKING REVISE; the second x86 surface):** αA's x86 census
  scoped the PRUNE to `src/x86_64/` ONLY (24 files / 847 LOC) and missed the FOLD-1 second surface
  (`ext/x86/` 3554 LOC vendored ASM + `build.rs` 102 LOC nasm driver + `Cargo.toml` nasm-rs dep +
  `lib.rs:247` ref; origin CH5 V3 §C.5/§F.7). αA was the LONE cohort artefact to retain the `src/`-scoped
  enumeration + close-gate (αC §6 FOLD-1, SYNTHESIS `:58-75`/`:246`/`:491`, HANDOFF inv-3 all carried
  crate-wide). FOLDED at V5 into §0 census x86 row, §3.2 x86 row, §5 PRUNE close-condition 4, §6 caveat,
  and the V5 FOLD log above — all now name BOTH surfaces and the crate-wide close-gate. This corrects the
  prior αA "ZERO V3 REVISE across all seven lenses" assertion (CH5 V3 carried a BLOCKING REVISE touching
  αA's x86 census). Verified at HEAD `318d9c046`: `ext/x86/` 4 files/3554 LOC; `build.rs` 102 LOC;
  `Cargo.toml` `build="build.rs"`:8 / `nasm-rs="0.3"`:19; `lib.rs:247` `ext/x86/bbnf.asm` ref; dormant on
  aarch64 (`build.rs:38-40` non-x86 early-return) → REVISE-not-REJECT but falsifies the literal "x86 gone".
- **V3 FOLD ledger** (the three V2-CH1 non-blocking notes on αA, each verified at HEAD
  `318d9c046`):
  - CH1 Note-1 (x86 LOC framing: 742 = `.rs`-only; `.asm` = 105; 847 total) → §3.2 + §0 amended to state
    both figures; the gate is content/file-count and is now **crate-wide** (V5 R-1), covering `ext/x86/`,
    `build.rs`, and `Cargo.toml` as well as `src/x86_64/`. Verified: 23 `.rs` (742 LOC) + 1 `.asm` (105 LOC) = 24.
  - CH1 Note-2 (working-tree caveat stale: the 7 replicas are NOW identical in the working tree
    too) → §6 caveat rewritten. Verified: working-tree md5 `sort -u` = 1; `f6a38445b` md5 `sort -u` = 1.
  - CH1 Note-3 ("no V1 CONSOLIDATED was produced" — αC/αD narration, NOT αA): not applicable to
    αA; recorded for completeness. (A `V1/CONSOLIDATED.md`, 7703 bytes, DOES exist on disk; αA
    never asserted otherwise.)

---

## §7 — One-paragraph synthesis (for αF)

SK-V17 stands EXACTLY on the inflection point: **JSON beats sonic-rs strict on all 51 rows
(+1.4%…+164.7%) and CSS beats lightningcss full-CSSOM 1.996×–3.348× cold on the real corpus,
both measurement-valid, over a genuinely unified tape/`ValueRef` substrate (Lock 1 holds).** But
the speed was bought with HAND-WRITTEN, FORKED parsers: CSS is a `const &str` literal scanner
(`runtime_generator.rs:701`), JSON is string-literal templates (`:195`), the generator forks on
`RuntimeEmitterKind` (`grammar_provider.rs:40`), the 7 CSS `generated.rs` are byte-identical (single
md5 at both `f6a38445b` and HEAD), `ValueRef`'s `G` axis is a phantom (only `AnyGrammar`; the `K`
axis is real for JSON), the value API is divergent (no shared trait), an x86 surface on TWO trees
(`src/x86_64/` 24 files = 23 `.rs` 742 LOC + 1 `.asm` 105 LOC, 14 `unimplemented!`; AND `ext/x86/`
3554 LOC vendored ASM + `build.rs` 102 LOC nasm driver + `Cargo.toml` nasm-rs dep + `lib.rs:247` ref —
≈ 4500 LOC, V5 R-1 fold) violates aarch64-only, an old
contrived warm micro-fixture bench survives (`nonjson_css_l4.rs:528,3091`), a metalang bench-id leaks
into shipped JSON (×7), and the CSS >SOTA is lazy-rich-summary vs eager-CSSOM (not equal-work).
SK-V18 is the backtrack: PRUNE the overfit (P1–P5), then GENERALIZE both parsers into ONE
grammar-driven generator (G1–G6 — G4 instantiating-or-deleting the `G` axis specifically), PROVE it
on a third grammar (Sheets, source `grammar/google-sheets/google-sheets.bbnf` brought into the
benched tree) via the generator only, **preserving every number in §1 and §2 from the
grammar-DERIVED parsers** — honestly framed (H1).
