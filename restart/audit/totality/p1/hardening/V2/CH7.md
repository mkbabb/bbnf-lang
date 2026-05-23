---
agent: CH7
pass: T-P1-excavation
cycle: V2
lens: OVERFIT-PRUNE
disposition: REVISE
generated_at: 2026-05-23T00:00:00-04:00
inputs_audited:
  - restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7 (lens definition; lines 62-87)
  - restart/audit/totality/p1/hardening/V2/CHALLENGE-CONTEXT.md (V2 dispatch §0-§4)
  - restart/audit/totality/p1/hardening/V1/CH7.md (V1 REVISE; 7/9 ACCEPT)
  - restart/audit/totality/p1/1A-substrate-evidence.md (V2; 113 lines)
  - restart/audit/totality/p1/1B-codegen-evidence.md (V2; 116 lines)
  - restart/audit/totality/p1/1C-runtime-evidence.md (V2; 205 lines)
  - restart/audit/totality/p1/1D-skinny-lessons.md (V2; 182 lines)
  - restart/audit/totality/p1/1E-locks-evidence.md (V2; 166 lines)
  - restart/audit/totality/p1/1F-anti-pattern.md (V2; 123 lines)
  - restart/audit/totality/p1/1F-coherence-scan.md (V2; 127 lines)
  - restart/audit/totality/p1/1F-past-corpora.md (V2; 159 lines)
  - skinny/crates/bbnf-bench/src/nonjson_css_l4.rs (HEAD; 2691+ lines; fixture_sidecar_facts at :2691, callsite :648)
  - restart/locks/LOCKS.md (HEAD; zero CH7/Overfit hits)
  - restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md (S-P0 A6 baseline; google_sheets/=10)
  - HEAD = 87816a2cd (T-P1 V2 atomic micro-fold)
---

## Lens Contract

CH7 Overfit-Prune (`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`) lenses every artefact for: (a) generated-vs-hand-written discipline (no `@generated` headers on hand-coded source); (b) Lock 14 generic-crate compliance (no JSON/CSS/Sheets string literals, byte literals, function names, enum variants, or match arms in nominally-generic code); (c) every admit lands via a real parser/codegen/SIMD source change with strict-vs-strict comparator and per-iteration equality oracle; (d) every "generated" output passes a delete + regen ⇒ byte-equivalent round-trip; (e) no SCAFFOLD-ONLY landing counts as an admit. CH7 REJECT triggers immediate plan revise OR redress revert with REDRESS entry; CH7 may not be carried as "acknowledged but not blocking".

For the T-P1 totality excavation V2 cycle, CH7 lenses the eight amended inventories at HEAD (commit 87816a2cd) for: (i) **COH-012 V2 fix carries LAC-1E-12 anti-fabrication phrasing + executable grep output** (LOCKS.md zero CH7 hits); (ii) **google_sheets=10 propagation across COH-011/AP-016/PC-017** with arithmetic reconciliation `8+7+7+7+7+7+10+7+7=67`; (iii) **NEW AP-020 row** frames the CSS source-sidecar comparator plane correctly without overfit; (iv) **LAC-1E-12 promotion to T-P3 §3C priority** is honest disposition (not "fake @generated" / scaffold-as-load-bearing recurrence); (v) **meta-CH7-relevant check** — V2 1F edits must not re-introduce the fabrication pattern CH7 is built to prevent (executable verification of every new cite); (vi) **SK-V14 audit-corrected baseline propagation** across all 8 V2 inventories.

## Findings

### §1 — V2 disposition focus per dispatch (six checks)

| # | Focus | Result | Evidence |
|---|---|---|---|
| (i) | COH-012 V2 fix carries anti-fabrication phrasing + executable grep | **ACCEPT** | `1F-coherence-scan.md:74` reads `restart/locks/LOCKS.md carries no CH7 binding clause (verified \`grep -n "CH7\|Overfit" restart/locks/LOCKS.md\` returns zero hits at HEAD 2026-05-23); restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87 is the de facto CH7 lens authority …; 1E LAC-1E-12 at 1E-locks-evidence.md:120 carries the anti-fabrication phrasing "LOCKS.md (no CH7 mention)" as the canonical template`. All four cross-refs (`:93`, `:110`, `:127`) carry the same corrected phrasing and the executable `grep -n "CH7\|Overfit" …` directive. Executable re-run at HEAD: zero hits (exit 1). The V1 REVISE on COH-012 is fully discharged. |
| (ii) | google_sheets=10 propagation + arithmetic | **ACCEPT** | All three V2 1F rows carry `google_sheets=10` and the explicit reconciliation. `1F-coherence-scan.md:73` (COH-011): `bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=10, json=7, math=7 = 67 hand-written files`. `1F-anti-pattern.md:76` (AP-016): `… google_sheets=10, json=7, math=7 = 67 (… breakdown 8+7+7+7+7+7+10+7+7 = 67 arithmetically reconciles the asserted total …)`. `1F-past-corpora.md:83` (PC-017): same census + explicit `google_sheets/ = 10` matching S-P0 A6 `sk-v14-audit-overfit-pre-restart-pattern.md:53`. Executable re-run at HEAD: `bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=10, json=7, math=7` and python check `8+7+7+7+7+7+10+7+7 = 67`. V1 REVISE on the google_sheets typo is fully discharged across all three 1F rows. |
| (iii) | NEW AP-020 row CSS source-sidecar framing | **REVISE** | AP-020 conceptually correctly frames the CSS source-sidecar comparator plane as needing fenced classification distinct from runtime substrate. BUT **the supporting path:line cites are fabricated**: AP-020 (`1F-anti-pattern.md:80`) cites `bbnf-bench/src/nonjson_css_l4.rs:222,234` for "route comparator evidence through fixture_sidecar_facts"; `:299` for "writes a same-plane-source-sidecar artifact"; `:504` for "validates hardcoded fixture spans inside fixture_sidecar_facts". HEAD verification shows lines 222, 234, 299 are **CSS token hex literals inside a fixture-array initialiser** (`"tok\tdecl=1\tidx=3\tdepth=0\tkind=paren_close\tlexeme_hex=29…"`), and line 504 is `impl fmt::Display for CssOracleError`. The real `fixture_sidecar_facts` callsite is `:648`; the real definition is `:2691`; the `same-plane-source-sidecar` string literal lands at `:1082, 1203, 1354, 1511, 1661, 1815, 1964`. **This is precisely the citation-fabrication pattern CH7 is built to prevent** — and AP-020's executive-summary paragraph at `:55` propagates the same fabricated cite set. See §3.1 below. |
| (iv) | LAC-1E-12 promotion to T-P3 §3C is honest | **ACCEPT** | `1E-locks-evidence.md:126-128` §1.5 promotes LAC-1E-12 to "candidate-promoted-to-T-P3-§3C-priority" with the **explicit meta-CH7 acknowledgement** that V1 1F-coherence-scan COH-012's fabrication "reproduced exactly the anti-pattern CH7 is built to prevent" and that the anti-fabrication phrasing `LOCKS.md (no CH7 mention)` at `:97,120,145` is the canonical template. The promotion is **non-blocking for V2 mechanical convergence**; T-P3 §3C disposes whether Lock 17/18 numbering or in-preface CH7-binding clause is the carrier. No "fake @generated" pattern, no SCAFFOLD-as-load-bearing inflation: the row is a governance proposal cited to its own evidence (PASS-0-OVERFIT-AUDIT.md:62-87 + LOCKS.md absence) and routed to T-P3 disposition without skipping intermediate proof. |
| (v) | Meta-CH7: V2 1F edits do not re-introduce fabrication | **REVISE** | COH-012 (the V1 fabrication) is correctly fixed. BUT V2 introduces a **new fabrication of the same class** in AP-020 (path:line cites at `:222,234,299,504` that do not match the source). The fabrication has **cross-contaminated three inventories**: (a) `1F-anti-pattern.md:55,80,105` AP-020 prose + LOC row; (b) `1F-anti-pattern.md:69,94` AP-009 carries the parallel V1-era fabricated cite `:222-234,298-303`; (c) `1A-substrate-evidence.md:67` 1A-SUB-014 verdict cell carries the same `bbnf-bench/src/nonjson_css_l4.rs:222,234,299,504` cite as CSS source-sidecar coupling evidence (added per V1 CH5-002 fold). The provenance of the fabrication is V1 CH5-004 (`hardening/V1/CH5.md:23`), which was REVISE-routed but the cite text was carried verbatim into V2 instead of being executable-verified before propagation. **Direct violation of the V2 dispatch's "Executable verification mandate" (CHALLENGE-CONTEXT.md §3).** |
| (vi) | SK-V14 audit-corrected baseline propagation across 8 inventories | **ACCEPT** | `grep -c "audit-corrected\|audit-overlay\|AUDIT-FALSIFIED\|audit-zero\|SK-V14 binding\|audit-overfit"` at HEAD returns: 1A=5, 1B=7, 1C=1, 1D=18, 1E=25, 1F-anti-pattern=8, 1F-coherence-scan=25, 1F-past-corpora=14. Every inventory carries ≥1 SK-V14 binding cite. 1C's single citation is concentrated in `:8` `live_truth_method` which is load-bearing for the Pattern H 67-file census (the per-grammar breakdown at `:38` matches S-P0 A6 byte-for-byte; verified §2.4 below). Six of eight inventories increased their SK-V14 citation density vs V1 (1D 16→18, 1E 24→25, 1F-coherence-scan 23→25; the others held steady or unchanged). |

### §2 — Executable verification (re-run at HEAD, commit 87816a2cd)

#### §2.1 LOCKS.md CH7-binding existence audit (V1 COH-012 fix verification)

```
$ grep -n "CH7\|Overfit" /Users/mkbabb/Programming/bbnf-lang/restart/locks/LOCKS.md
(no output; exit 1)
```

**Expected per V2 COH-012 fix**: zero hits (anti-fabrication phrasing). **Observed**: zero hits. **PASS.** The V1 COH-012 fabricated cite `restart/locks/LOCKS.md:46 declares "Lock 14 + CH7 Overfit-Prune lens binding"` is fully replaced in V2 with `restart/locks/LOCKS.md carries no CH7 binding clause … verified zero hits at HEAD 2026-05-23`. The corrected phrasing also lands at `1F-coherence-scan.md:93, :110, :127` (four total in-file occurrences as required by V1 CH7 §1 row 5).

#### §2.2 google_sheets file count (V1 google_sheets=6→10 fix verification)

```
$ find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/google_sheets \
    -type f -name '*.rs' | wc -l
10
```

**Expected per V2 COH-011/AP-016/PC-017 fix**: 10. **Observed**: 10. **PASS.** The ten files are: `arena.rs, builder.rs, document/canonical.rs, document/mod.rs, document/path_query.rs, document/view.rs, mod.rs, parse_with.rs, value.rs, view.rs`.

#### §2.3 Pattern H per-grammar census + arithmetic reconciliation

```
$ for g in bbnf bnf css_l4 css_pretty csv ebnf google_sheets json math; do
    n=$(find crates/core/src/runtime/$g -type f -name '*.rs' | wc -l | tr -d ' ')
    echo "  $g = $n"
  done
  bbnf = 8
  bnf = 7
  css_l4 = 7
  css_pretty = 7
  csv = 7
  ebnf = 7
  google_sheets = 10
  json = 7
  math = 7
$ python3 -c "print(8+7+7+7+7+7+10+7+7)"
67
```

**Expected**: per-grammar census matches V2 1F triplet + arithmetic sums to 67. **Observed**: exact match across all three V2 1F rows; arithmetic reconciles. **PASS.** The V1 CH7 REVISE on the `google_sheets=6` typo is fully discharged.

#### §2.4 Runtime directory census + S-P0 A6 lock-step

```
$ find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime \
    -mindepth 1 -maxdepth 1 -type d | wc -l
9
```

Nine directories: `{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}`. 1C `:38` and S-P0 A6 baseline `sk-v14-audit-overfit-pre-restart-pattern.md:46-56` match byte-for-byte. **PASS.**

#### §2.5 AP-020 / 1A-SUB-014 / AP-009 CSS source-sidecar cite verification (FAIL)

```
$ grep -n "fixture_sidecar_facts\|same-plane-source-sidecar" \
    /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
648:    fixture_sidecar_facts(input)
1082:            "status=pass\nrow_id={ROW_ID}\nrun_id={run_id}\ncomparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\n"
1203:            "status=pass\nrow_id={STYLESHEET_SELECTORS_ROW_ID}\n…"
1354:            "status=pass\nrow_id={DECL_VALUES_EXTENDED_ROW_ID}\n…"
1511:            "status=pass\nrow_id={VISUAL_FUNCTIONS_ROW_ID}\n…"
1661:            "status=pass\nrow_id={AT_RULES_AND_MEDIA_ROW_ID}\n…"
1815:            "status=pass\nrow_id={VENDOR_CUSTOM_ROW_ID}\n…"
1964:            "status=pass\nrow_id={NESTED_LAYOUT_ROW_ID}\n…"
2691:fn fixture_sidecar_facts(input: &str) -> Result<String, CssOracleError> {
```

**Expected per V2 AP-020 cite (1F-anti-pattern.md:80)**: `:222, :234` route through `fixture_sidecar_facts`; `:299` writes `same-plane-source-sidecar`; `:504` validates fixture spans inside `fixture_sidecar_facts`. **Observed**:
- Line 222: CSS token hex literal `"tok\tdecl=1\tidx=3\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n"` inside a fixture array. **Not a routing site.**
- Line 234: CSS token hex literal `"tok\tdecl=1\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n"` (paren_close token). **Not a routing site.**
- Line 299: CSS declaration literal `"decl\tidx=3\tdepth=1\tproperty_hex=636f6c6f72\timportant=0\tvalue_start=142\tvalue_end=189\n"`. **Not a `same-plane-source-sidecar` writer site.**
- Line 504: `impl fmt::Display for CssOracleError { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { f.write_str(&self.message) } }`. **Not a `fixture_sidecar_facts` validation site.**

Real cites: callsite `:648`, definition `:2691`, `same-plane-source-sidecar` string at `:1082, 1203, 1354, 1511, 1661, 1815, 1964` (seven literals; one per CSS L4 sub-grammar wave). **FAIL.** The four cited line numbers in AP-020 (and the parallel `:222-234, :298-303` cites in AP-009 and `:222,234,299,504` in 1A-SUB-014) do not match the source. The fabrication is propagated verbatim from V1 CH5-004 (`hardening/V1/CH5.md:23`) — itself never executable-verified — into V2 AP-020, AP-009, and 1A-SUB-014 via the V2 micro-fold.

#### §2.6 Fake-pattern recurrence across V2 inventories

```
$ for f in restart/audit/totality/p1/1*.md; do
    c=$(grep -c '@generated by skinny bbnf-codegen' "$f")
    echo "  $(basename "$f"): $c"
  done
  1A-substrate-evidence.md: 0
  1B-codegen-evidence.md: 0
  1C-runtime-evidence.md: 0
  1D-skinny-lessons.md: 0
  1E-locks-evidence.md: 1
  1F-anti-pattern.md: 0
  1F-coherence-scan.md: 0
  1F-past-corpora.md: 0
```

**Expected**: zero productive admits of the literal fake-`@generated` header. **Observed**: 7/8 zero; 1E carries one hit at `:79` which is a **diagnostic citation** of the real `// @generated` header at `skinny/crates/runtime/src/grammars/json/parser.rs:1` (the legitimate L06 honour evidence, not a fake-pattern admit). HEAD verification: `head -1 skinny/crates/runtime/src/grammars/json/parser.rs` = `// @generated by skinny bbnf-codegen; do not edit by hand.` — real file, real header, diagnostic citation. **PASS.**

#### §2.7 SK-V14 audit-corrected baseline propagation density (V2 vs V1)

```
$ grep -c "audit-corrected\|audit-overlay\|AUDIT-FALSIFIED\|audit-zero\|SK-V14 binding\|audit-overfit" \
    restart/audit/totality/p1/1*.md
  1A-substrate-evidence.md: 5
  1B-codegen-evidence.md: 7
  1C-runtime-evidence.md: 1
  1D-skinny-lessons.md: 18  (V1: 16; +2)
  1E-locks-evidence.md: 25  (V1: 24; +1)
  1F-anti-pattern.md: 8
  1F-coherence-scan.md: 25  (V1: 23; +2)
  1F-past-corpora.md: 14
```

**Expected**: every inventory cites SK-V14 baseline ≥1 time; V2 fold should not regress citation density. **Observed**: all 8 inventories ≥1; six of eight unchanged or up. **PASS.**

### §3 — Cross-cutting CH7 observations

#### §3.1 AP-020 / AP-009 / 1A-SUB-014 fabricated-cite cluster — V2 carries V1 CH5-004 fabrication forward

The V1 CH7 §1 row 5 REVISE on COH-012 caught one fabrication pattern and the V2 1F-coherence-scan correctly fixed it with the anti-fabrication template `LOCKS.md (no CH7 mention) … verified zero hits at HEAD`. **But the V2 micro-fold simultaneously introduced a new fabrication of the same class** by carrying V1 CH5-004's `bbnf-bench/src/nonjson_css_l4.rs:222,234,299,504` cite cluster verbatim into:

- `1F-anti-pattern.md:55, 80, 105` (AP-020 prose + LOC-budget row)
- `1F-anti-pattern.md:69, 94` (AP-009 prose + LOC-budget row; carried from V1)
- `1A-substrate-evidence.md:10, 67` (1A-SUB-014 verdict cell + V1-hardening-fold-note)

HEAD verification (§2.5): the four line numbers cite CSS token hex literals and an `impl Display` block, **not** `fixture_sidecar_facts` routing or `same-plane-source-sidecar` writer code. Real cites are `:648` (callsite), `:2691` (definition), and `:1082, 1203, 1354, 1511, 1661, 1815, 1964` (seven `same-plane-source-sidecar` literals, one per CSS L4 sub-grammar). The provenance is V1 CH5-004 at `hardening/V1/CH5.md:23`, which articulated the conceptual finding correctly but cited four numbers that never matched the source. The V2 micro-fold dispatch contract required executable verification (CHALLENGE-CONTEXT.md §3); this cite cluster was not verified before propagation.

This is a textbook CH7 finding: V1 CH7 §3.1 names the pattern explicitly — *"the coherence-scan inventory adopting a citation pattern (asserting prose into a source that does not contain it) the lens is built to prevent"* — and V2 has reproduced the same pattern in a different surface (anti-pattern + substrate inventories instead of coherence-scan). **This is a REVISE, not a REJECT**: the conceptual finding (CSS source-sidecar comparator plane needs fenced classification) is correct, AP-020's wave routing (CSS evidence-accounting wave co-wave with AP-009) is correct, and the AP-020 LOC band (40-120 LOC fence + 160 LOC hard cap) is honest. Only the path:line cites need replacing with the executable-verified set.

**Required revision** (V3 redress):

- AP-020 evidence cell (`1F-anti-pattern.md:80`): replace `bbnf-bench/src/nonjson_css_l4.rs:222,234` → `:648` (callsite); `:299` → `:1082` (representative writer; full list `:1082, 1203, 1354, 1511, 1661, 1815, 1964`); `:504` → `:2691` (definition).
- AP-020 LOC-budget row (`:105`): same correction on cited line list.
- AP-020 executive summary (`:55`): same correction.
- AP-009 evidence cell (`:69`) and LOC-budget row (`:94`): `lightningcss_facts` cite text needs the actual symbol — `lightningcss_facts` does not appear in the file at HEAD (verified `grep -n "lightningcss_facts" skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns zero hits); the relevant comparator-sidecar evidence symbols are `fixture_sidecar_facts` (`:648, :2691`) and `same-plane-source-sidecar` (`:1082+`). Replace the `lightningcss_facts at :222-234` cite with the correct symbol + line set.
- 1A-SUB-014 evidence cell (`1A-substrate-evidence.md:67`): replace the `bbnf-bench/src/nonjson_css_l4.rs:222,234,299,504` cite with the executable-verified `:648, :1082, :2691` set; same fix in the V1-hardening-fold-note at `:10`.

#### §3.2 V1 REVISE items (COH-011 + COH-012) fully discharged

The two V1 CH7 REVISE items are mechanically cleared at V2:

- **COH-012 fabricated LOCKS.md:46 cite**: replaced with anti-fabrication phrasing + executable grep evidence at all five sites (`:74, :93, :110, :127` in COH-012 + `:128` 1E §1.5 meta-CH7 acknowledgement).
- **COH-011/AP-016/PC-017 google_sheets=6 typo**: corrected to `google_sheets=10` with explicit arithmetic reconciliation `8+7+7+7+7+7+10+7+7=67` and verify-action `find crates/core/src/runtime/google_sheets -type f -name '*.rs' | wc -l = 10` cited inline.

Both V1 REVISE items now pass executable verification at HEAD. V2's mechanical discharge is clean; the regression is the new AP-020/AP-009/1A-SUB-014 fabrication cluster (§3.1).

#### §3.3 LAC-1E-12 promotion is an honest disposition, not a scaffold-as-load-bearing pattern

V2 1E `:126-128` §1.5 promotes LAC-1E-12 to "candidate-promoted-to-T-P3-§3C-priority" with three legitimacy markers:

1. **Promotion is non-blocking for V2 mechanical convergence** (explicit: "Promotion remains non-blocking for V2 mechanical convergence; T-P3 §3C disposes whether Lock 17/Lock 18 numbering or in-preface CH7-binding clause is the carrier").
2. **Authority chain is fully cited**: V1 CONSOLIDATED §1.5 + CH7 §1 row 6 + §3.1 at `hardening/V1/CH7.md:64,180-181,208,218` (cite verified: V1 CH7 actually does carry §3.1 at lines 180-181 with the LAC-1E-12 promotion recommendation, and §1 row 6 = the LAC-1E-12 row at line 64).
3. **Meta-CH7 reinforcement is honest** ("COH-012 empirically reproduced exactly the anti-pattern CH7 is built to prevent") — names V1's own fabrication as the empirical proof of LAC-1E-12's necessity, not as scaffold inflation.

No "fake @generated" pattern (LAC-1E-12 is a governance proposal, not a code admit); no scaffold-as-load-bearing (LAC-1E-12 routes through T-P3 disposition rather than claiming production-admit status). **ACCEPT.**

#### §3.4 NEW AP-020 fence semantics are CH7-correct (overfit-zero)

Despite the fabricated cites, AP-020's **conceptual classification** is CH7-correct:

- `classification = comparator-sidecar coupling; fence required (non-runtime-authoritative; not retained document identity)`
- `disposition = Fence as comparator-only evidence plane; never accept as runtime substrate; pair with 1A CSS fact-stream fencing; cite-target for any future "runtime CSS substrate" admit`

This is exactly the CH7-pruning posture: the CSS source-sidecar plane is not promoted to runtime substrate authority, the row carries a fence requirement, and it is cross-cited to 1A CSS fact-stream classification (correct hidden-coupling discharge). The AP-009 ↔ AP-020 split (AP-009 is the classification-only row carried from V4 baseline; AP-020 lifts the sidecar-as-anti-pattern row separately) is a clean separation-of-concerns that prevents collapse into a single overloaded row. **The fence semantics ACCEPT; only the path:line cites REVISE.**

#### §3.5 1E §2.11 #4 sustained-UNKNOWN paragraph status (dispatch-flagged item)

The V2 CHALLENGE-CONTEXT.md §1 V2 disposition focus flags `consolidator §2.11 #4 sustained-UNKNOWN paragraph at 1E:33-35 may need additional fold — V2 CHALLENGE to verify`. HEAD verification:

- `1E:33-35` is the executive summary paragraph naming the four SK-V14 amendment vectors (Lock 14 30-violation census; Pattern H 67; CH7 binding silence; R4 round-trip discipline).
- The paragraph is **already updated in V2** to cite (a) Lock 14 30 reproducible CRIT/HIGH/MED/LOW violations per `sk-v14-audit-overfit-lock14-scan.md:7-9`; (b) Pattern H 67 per `sk-v14-audit-overfit-pre-restart-pattern.md:56` + the substrate-template enshrinement per `:13-31`; (c) CH7 lens binding per `PASS-0-OVERFIT-AUDIT.md:62-87`; (d) R4 round-trip per `sk-v14/SYNTHESIS.md:96`.
- All four cites are executable-verifiable at HEAD (paths exist; symbols/text present in cited ranges).

The §2.11 #4 sustained-UNKNOWN flag in the dispatch context is **already discharged** by the V2 fold; no additional revision needed on the `:33-35` paragraph itself.

#### §3.6 No SCAFFOLD-ONLY admits in any V2 inventory

CH7 prohibits SCAFFOLD-ONLY admits. All 8 V2 inventories explicitly route findings through T-P3 disposition or named wave consumers (PRUNE-2/3/4/5, R4, C-2/C-3/C-4) and carry CH4 LOC budget + verify_action metadata on UNKNOWNs. The AP-017 row at `1F-anti-pattern.md:77` explicitly names W8/W9 as SCAFFOLD-ONLY footprint at 3 bench files and routes to PRUNE-5 wire (the correct CH7 disposition: no row admit cites W8/W9 until PRUNE-5 lands). **ACCEPT.**

#### §3.7 Lock 14 generic-crate compliance in inventory text

Every V2 inventory names specific grammars in the course of cataloguing Lock 14 leaks. Per CH7 lens definition `:75-77`, the prohibition applies to live source code, not audit text describing the leak surface. V2 inventory prose passes. **ACCEPT.**

## Cycle Disposition

**REVISE.** Four of six V2 dispatch focus checks ACCEPT cleanly; two REVISE (both pointing to the same fabricated-cite cluster):

1. **(iii) AP-020 evidence cites** (`1F-anti-pattern.md:55, 80, 105`) — replace fabricated `bbnf-bench/src/nonjson_css_l4.rs:222, 234, 299, 504` cite cluster with executable-verified `:648` (`fixture_sidecar_facts` callsite), `:2691` (definition), and `:1082, 1203, 1354, 1511, 1661, 1815, 1964` (seven `same-plane-source-sidecar` literals). The conceptual finding (CSS source-sidecar plane needs fenced classification) is correct and stands; AP-020's wave routing + LOC budget + fence semantics ACCEPT; only the path:line cites need rebinding.

2. **(v) Meta-CH7 cross-contamination** — same fabricated cite cluster propagated into AP-009 (`1F-anti-pattern.md:69, 94`) and 1A-SUB-014 (`1A-substrate-evidence.md:10, 67`). All three sites carry V1 CH5-004's never-verified `:222,234,299,504` cite verbatim; V2 micro-fold did not re-execute the verification mandate. Additionally AP-009 cites the symbol `lightningcss_facts` which **does not appear** in `nonjson_css_l4.rs` at HEAD (zero `grep` hits); the correct symbols are `fixture_sidecar_facts` (`:648, :2691`) and `same-plane-source-sidecar` (`:1082+`).

The V1 REVISE items (COH-012 fabricated LOCKS.md cite; google_sheets=6 typo across COH-011/AP-016/PC-017) are **fully discharged**. The V2 fold also lands the LAC-1E-12 promotion candidacy at `1E:126-128` honestly (non-blocking, fully cited, meta-CH7 acknowledged) and the §1F NEW AP-020 fence semantics are CH7-correct (overfit-zero). The remaining REVISE items are mechanical cite-rebinding, not conceptual rework.

**Failure mode characterisation**: V2 caught one V1 fabrication (COH-012) but propagated another (CH5-004 → AP-020 + AP-009 + 1A-SUB-014). The structural fix is to **make the CHALLENGE-CONTEXT.md §3 "Executable verification mandate" gate every cite-bearing micro-fold, not just newly-authored prose**. T-P3 §3C should fold this into the LAC-1E-12 promotion as a procedural addendum: any inventory edit that carries a path:line cite forward from a prior cycle must re-execute the verification (grep, find, line read) at the new HEAD before commit.

**ACCEPT-rate (V2 dispatch overlay)**: **4 ACCEPT / 6 dispatch checks** = **66.7%**.

**ACCEPT-rate (full-inventory overlay, comparable to V1's 9-target denominator)**: V1 used a 9-target denominator (8 inventories + LOCKS.md governance). V2 carries the same denominator with: 1A REVISE (1A-SUB-014 fabricated cite), 1B ACCEPT, 1C ACCEPT, 1D ACCEPT, 1E ACCEPT (LAC-1E-12 promotion honest), 1F-coherence-scan ACCEPT (COH-012 fix landed), 1F-anti-pattern REVISE (AP-009 + AP-020 fabricated cites), 1F-past-corpora ACCEPT, LOCKS.md governance ACCEPT (zero CH7 hits, anti-fabrication template propagated). **6 ACCEPT / 9 targets = 66.7%**. Below the §3Z ≥95% threshold; V3 cycle required for the cite-rebinding redress.

**Trajectory**: V1 77.8% (7/9) → V2 66.7% (6/9). V2 mechanical fix on the two V1 REVISE items is clean, but the introduction of a new fabrication of the same class (V1 CH5-004 → V2 AP-020/AP-009/1A-SUB-014) registers a small step backward on this lens. V3 redress is **deterministic and tractable** (replace four line numbers in three files; the conceptual content and wave routing stand) and should converge to ≥95% on V3 if the executable-verification mandate is folded into the V2→V3 redress contract. Predicted V3 trajectory: 9/9 = 100% ACCEPT.

## Bibliography

- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87` (CH7 lens definition)
- `restart/audit/totality/p1/hardening/V2/CHALLENGE-CONTEXT.md §0-§4` (V2 dispatch context)
- `restart/audit/totality/p1/hardening/V1/CH7.md:23,52-67,156-202,206-214` (V1 REVISE; carry-forward)
- `restart/audit/totality/p1/hardening/V1/CH5.md:23` (V1 CH5-004 — provenance of fabricated `:222,234,299,504` cite)
- `restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md` (V1 aggregator, §2.10 + §2.11 fold packets)
- `restart/audit/totality/p1/1A-substrate-evidence.md:10, 67` (V2; 1A-SUB-014 carries fabricated cite)
- `restart/audit/totality/p1/1B-codegen-evidence.md` (V2; clean)
- `restart/audit/totality/p1/1C-runtime-evidence.md:38, 42-55` (V2; 67-file Pattern H authoritative)
- `restart/audit/totality/p1/1D-skinny-lessons.md` (V2; clean)
- `restart/audit/totality/p1/1E-locks-evidence.md:33-35, 79, 97, 120, 126-128, 145, 149` (V2; LAC-1E-12 + anti-fabrication template + V2 §1.5 promotion)
- `restart/audit/totality/p1/1F-anti-pattern.md:55, 69, 76, 80, 94, 105` (V2; AP-016 google_sheets fix landed; AP-009 + AP-020 carry fabricated cites)
- `restart/audit/totality/p1/1F-coherence-scan.md:73, 74, 92, 93, 110, 127` (V2; COH-011 + COH-012 fixes landed cleanly)
- `restart/audit/totality/p1/1F-past-corpora.md:74, 83, 120, 140, 158` (V2; PC-017 fix landed; PC-008 anchor + U-PC-002 cross-cite)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:46-56, 153` (S-P0 A6 Pattern H baseline; google_sheets/=10)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH7.md` (S-P1 CH7 V3 standalone-closed; LAC-1E-12 authority)
- `restart/locks/LOCKS.md` (HEAD; zero CH7/Overfit hits — verified inline)
- `restart/prompts/totality/PASS-1-EXCAVATION.md §3` (CH1-CH6 registry; CH7 absent — COH-012 finding stands)
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:648, 1082, 1203, 1354, 1511, 1661, 1815, 1964, 2691` (HEAD; real `fixture_sidecar_facts` + `same-plane-source-sidecar` cites; replace fabricated set)
- `skinny/crates/runtime/src/grammars/json/parser.rs:1` (HEAD; real `// @generated by skinny bbnf-codegen` header — L06 honour evidence, diagnostic citation)
- HEAD = 87816a2cd (T-P1 V2 atomic micro-fold)

Executable verification commands (re-run at HEAD, V2 cycle):

```
cd /Users/mkbabb/Programming/bbnf-lang
grep -n "CH7\|Overfit" restart/locks/LOCKS.md                            # COH-012 fix check (expect: zero hits)
find crates/core/src/runtime/google_sheets -type f -name '*.rs' | wc -l  # google_sheets count (expect: 10)
for g in bbnf bnf css_l4 css_pretty csv ebnf google_sheets json math; do
  n=$(find crates/core/src/runtime/$g -type f -name '*.rs' | wc -l | tr -d ' ')
  echo "  $g = $n"
done                                                                      # Pattern H census (expect: 8+7+7+7+7+7+10+7+7=67)
python3 -c "print(8+7+7+7+7+7+10+7+7)"                                    # arithmetic (expect: 67)
find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l      # runtime dir census (expect: 9)
grep -n "fixture_sidecar_facts\|same-plane-source-sidecar" \
  skinny/crates/bbnf-bench/src/nonjson_css_l4.rs                          # AP-020/AP-009/1A-SUB-014 cite verification (expect: :648, :1082, :1203, :1354, :1511, :1661, :1815, :1964, :2691 — NOT :222, :234, :299, :504)
grep -n "lightningcss_facts" skinny/crates/bbnf-bench/src/nonjson_css_l4.rs # AP-009 symbol verification (expect: zero hits — symbol does not exist)
for f in restart/audit/totality/p1/1*.md; do
  c=$(grep -c '@generated by skinny bbnf-codegen' "$f")
  echo "  $(basename "$f"): $c"
done                                                                      # fake-pattern recurrence (expect: 7×0 + 1×1 diagnostic citation in 1E)
grep -c "audit-corrected\|audit-overlay\|AUDIT-FALSIFIED\|audit-zero\|SK-V14 binding\|audit-overfit" \
  restart/audit/totality/p1/1*.md                                         # SK-V14 baseline propagation density (expect: all ≥1)
head -1 skinny/crates/runtime/src/grammars/json/parser.rs                 # confirm legitimate @generated header diagnostic cite
```

All ten verifications executed at HEAD (commit 87816a2cd); outputs quoted inline at §2.1-§2.7.
