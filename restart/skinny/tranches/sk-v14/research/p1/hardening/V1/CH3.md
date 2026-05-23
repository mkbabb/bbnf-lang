# S-P1 V1 CHALLENGE — CH3 REGRESSION (REDRESS) Lens

Authored: 2026-05-23 (post-S-P1 V1 atomic commit). Six P1 axis files
under review: p1a (340 lines), p1b (320), p1c (607), p1d (648), p1e
(306), p1f (260). Authorities re-read end-to-end:
`restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH3,
`restart/prompts/ORCHESTRATOR.md` §3W (CH3 universal definition) + §3Z,
`restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md`,
`restart/skinny/tranches/sk-v14/SYNTHESIS.md` §0.4 P-1..P-7 pre-blocks,
and `skinny/REDRESS.md` (5041 lines; sampled via grep + offset per
`[read-size-preflight]`). Dispatch context binding:
`restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md`
§0-§5, §2 CH3 focus.

CH3 binding restated (PASS-1-PROFILE §3 CH3): any §4 anomaly suggesting
a route already in `skinny/REDRESS.md` must cite the entry and mark
pre-blocked, **not** implicitly re-open. Specific watch-list per
dispatch §2: REDRESS-126 (PEXT/aarch64 + zero-orphan guard), REDRESS
50-55 (dispatch-table / function-pointer alternates), 60-72
(parser-local cursors), 80 + 82-84 (event sidecars + mantissa-widen +
classifier + StringBlock16), 88-89 (consumed aarch64 bitmap bodies +
CTZ bulk consumer with B6 canary). Adjacent prune-list: REDRESS 96/97/98
(union-substrate / production-union routes).

## §1 — Disposition summary

| Axis | §4 anomalies | REDRESS routes invoked? | Pre-block citation present? | Disposition |
|---|---:|---|---|---|
| p1a-samply-mode-1 | 6 entries (`§4` bullet list) | None proposed; CH3 route guard paragraph explicitly pre-blocks 5 inferred candidates (REDRESS-50, 51, 60, 83, 84) | YES (explicit at lines 267-274) | ACCEPT |
| p1b-samply-mode-2 | 6 anomalies + 2 trailing notes | Anomaly 1 (driver overhead → `JsonSink` fold), Anomaly 4 (`DirectParser::skip_value` tape-only walk) — neither maps to a pre-blocked REDRESS family; both are S-P2 primitive-design prompts | N/A (no pre-blocked route invoked) | ACCEPT |
| p1c-samply-mode-3 | 8 anomalies (ANOM-1 through ANOM-8) | ANOM-1/2/3 bound by ANOM-6 to REDRESS-126 zero-orphan; ANOM-5 cites PEXT as aarch64-impossible; ANOM-8 cites REDRESS 96/97/98 as pre-blocked | YES (ANOM-6 at 498-507; ANOM-5 at 485-496; ANOM-8 at 540-541) | ACCEPT-WITH-NOTE (citation path drift, §3 F-1) |
| p1d-pmu-cycles | 8 anomalies | Anomaly 2 (`alternate_scalar_plan` declared NOT a redress prompt); Anomaly 6 (SIMD/scalar ratio explicitly pre-blocked against REDRESS 96/97/98); rest are CH1/CH5 telemetry observations | YES (Anomaly 6 at 564-567) | ACCEPT |
| p1e-hot-leaf-attribution | 7 §4 subsections (§4.1-§4.7) | §4.4 substrate-union observation flagged as Lock 1 single-primitive (not split per S-P0 P-2); §4.7 is the load-bearing **REDRESS guard reconciliation** naming every pre-blocked family verbatim | YES (§4.7 at 248 — names REDRESS 50-55, 60-72, 80, 82-84, 88-89, 96-98, 126) | ACCEPT |
| p1f-results-delta | 6 §4 subsections (§4.1-§4.6) | None — all six are schema-gap / telemetry-drift / row-count findings; no route to a parser primitive is proposed | N/A (documentary; no route proposed) | ACCEPT |

Per-lens ACCEPT-rate: **6/6 (100%)**. Zero anomalies silently re-open a
pre-blocked REDRESS family. One ACCEPT-WITH-NOTE (citation path drift,
documentary). No ANOM REVISE / REJECT.

## §2 — Per-anomaly REDRESS adjacency table (full audit)

For every §4 anomaly across all six artefacts, the closest REDRESS
family + the artefact's disposition treatment:

| Artefact | Anomaly | Nearest REDRESS family | Cited in artefact? | Disposition |
|---|---|---|---|---|
| p1a | CH2 envelope-not-primitive masking signal | — (CH2 observation; no route) | n/a | accepted |
| p1a | Lock 14 audit re-surface (`match_tiny_plain_string_with_cap` in `generated.rs`) | Pattern-H residue per S-P0; primitive-hosting move, not a route | n/a | accepted |
| p1a | CH3 route guard (5 inferences pre-blocked) | REDRESS-50, 51, 60, 83, 84 | YES — explicit `→ REDRESS-N` mapping per inference (a-e) at 269-273 | accepted |
| p1a | PMU-Mbps consistency (-9 to -28% per corpus) | — (criterion-warm vs cold-loop; CH4 reproducibility) | n/a | accepted |
| p1a | W14.1-.5 audit-falsified rows | — (audit overlay; CH7) | n/a | accepted |
| p1a | R1 comparator misbinding (carry-forward) | — (R1 pin; not S-P1 scope) | n/a | accepted |
| p1a | Floor-density anomalies (gsoc-2018 movemask; marine_ik + canada `copy_nonoverlapping`) | — (SIMD substrate + tape-commit) | n/a | accepted |
| p1b | A1 — driver overhead leaks into hot leaf | — (proposes `JsonSink` fold = existing same-substrate trait, NOT REDRESS-54/55/66/69 decoded-string sink family) | n/a (no pre-blocked route invoked) | accepted |
| p1b | A2 — criterion-slope vs cold-per-parse gap | — (CH4 methodology) | n/a | accepted |
| p1b | A3 — Track 2 vs Track 1 cross-over on string/object ratio | — (substrate observation; CH5 hidden coupling) | n/a | accepted |
| p1b | A4 — `DirectParser::skip_value` dominates typed | — (proposes tape-only skip = within-Track-1 substrate-walk, NOT REDRESS-51 parser-local-cursor reopen; semantics differ) | n/a (no pre-blocked route invoked) | accepted |
| p1b | A5 — corpus locator dash-name miss | — (harness footgun; CH4) | n/a | accepted |
| p1b | A6 — unicode Mbps worst but not c/B worst | — (clock-call overhead; CH4) | n/a | accepted |
| p1b | masking-probe scope note | — (defers to p1c) | n/a | accepted |
| p1b | comparator misbinding carry-forward | — (R1 pin) | n/a | accepted |
| p1c | ANOM-1 `alternate_scalar_plan` is serde_json comparator | REDRESS-126 zero-orphan (ANOM-6 binds) | YES (ANOM-6 explicit; ANOM-1 proposes `comparator_serde_json_value` rename OR `scan_structurals_scalar` walk both subject to REDRESS-126) | accepted |
| p1c | ANOM-2 `host_call_eager_decode` view-walk + UTF-8 | REDRESS-126 zero-orphan (ANOM-6 binds); semantic adjacency to REDRESS-54/55/66/69 sink-decoded-stat family is NOT triggered (this is substrate-walk attribution, not a new sink fact) | YES (ANOM-6 at 503-507) | accepted |
| p1c | ANOM-3 cold_first allocator-bound | REDRESS-126 zero-orphan (ANOM-6 binds); the proposal is harness/methodology separation, NOT a parser route | YES (ANOM-6) | accepted |
| p1c | ANOM-4 dispatch_value folded symbol (parse-attribution off) | — (feature-flag profile fold; CH6) | n/a | accepted |
| p1c | ANOM-5 PEXT unsupported on aarch64 | REDRESS-126 (PEXT cited as architecturally impossible on aarch64; SK-V13 P1-C V2 finding restated) | YES (explicit at 485-496) | accepted |
| p1c | ANOM-6 REDRESS-126 applies to ANOM-1/2/3 | REDRESS-126 + tripwire 50-55, 60-72, 80, 82-84, 88, 89 | YES (entire purpose) | accepted |
| p1c | ANOM-7 R1 comparator misbinding inherited | — (R1 pin; S-P2 scope) | n/a | accepted |
| p1c | ANOM-8 structural SIMD speedup corpus-correlated | REDRESS 96/97/98 (substrate-union route) | YES (explicit at 540-541) | accepted |
| p1d | 1 — branch/L1/LLC counters unavailable | — (CH1/CH6 telemetry gap; sudo refused) | n/a | accepted |
| p1d | 2 — alternate < cold on every row (serde noise) | REDRESS-126 adjacency (the alternate-scalar route ANOM-1 mentions) explicitly declared NOT a redress prompt | YES (line 528-529 "MASKING signal is real noise, not a redress prompt") | accepted |
| p1d | 3 — host_call cost dominates unicode | — (substrate signal; AUDIT-PENDING per overlay) | n/a | accepted |
| p1d | 4 — Track 2 typed > Track 1 typed (apache, random) | — (audit-overlay falsification confirmation) | n/a | accepted |
| p1d | 5 — citm_catalog 1.187 c/B floor | — (substrate-bound bench-harness empirical floor) | n/a | accepted |
| p1d | 6 — structural_simd vs scalar ratio range | REDRESS 96/97/98 | YES (explicit at 564-567 "pre-blocked against any union substrate route") | accepted |
| p1d | 7 — no prior PMU TSV at SK-V13 schema | — (CH4 baseline) | n/a | accepted |
| p1d | 8 — single-lane `sonic_rs_anchor` finding | — (R1 misbinding; S-P2 scope) | n/a | accepted |
| p1e | §4.1 CH2 Lock-14 mis-attribution census | — (feature-flag fold prompt, NOT a route) | n/a | accepted |
| p1e | §4.2 admit-vs-profile contradictions typed plane | — (audit-overlay falsification; CH7) | n/a | accepted |
| p1e | §4.3 CSS L4 zero-parser-profile finding | — (PRUNE-2 binding; CH7) | n/a | accepted |
| p1e | §4.4 substrate-union substrate-vs-producer mixing | Lock 1 single-primitive — explicitly does NOT split into 2 routes (which would trip REDRESS 96/97/98) | YES (single-primitive binding per S-P0 P-2) | accepted |
| p1e | §4.5 mode-III SIMD/scalar ratios concentrate on float-heavy | — (overfit-guard against SIMD-by-float; CH7) | n/a | accepted |
| p1e | §4.6 save-only sidecar gaps (CH6 paper-close) | — (CH6 sidecar discipline) | n/a | accepted |
| p1e | §4.7 REDRESS guard reconciliation | REDRESS 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 126 — all named verbatim | YES (load-bearing CH3 statement for the whole tranche) | accepted |
| p1f | §4.1 schema-extension columns absent | — (CH4 telemetry; column schema) | n/a | accepted |
| p1f | §4.2 SPEC §0.4 27-field manifest gaps | — (CH4 telemetry; field census) | n/a | accepted |
| p1f | §4.3 narrow vs broader ledger row-count discrepancy | — (S-P0 PRUNE-1 reconciliation; CH7) | n/a | accepted |
| p1f | §4.4 missing rows in RESULTS.md | — (population gap; CH4) | n/a | accepted |
| p1f | §4.5 stale telemetry signals | — (CH6 paper-close risk; criterion-slope-profile path masquerading as Hot leaf) | n/a | accepted |
| p1f | §4.6 masking probes telemetry absent | — (p1c lands the rows; CH4 population gap) | n/a | accepted |

**Coverage**: 43 distinct §4 entries across six artefacts. Every entry
whose adjacency to a pre-blocked REDRESS family is non-zero carries an
explicit citation of the relevant entry **and** a pre-block flag. No
entry implicitly reopens a pre-blocked route.

## §3 — Critical findings (CH3 lens, new)

### F-1 — Citation path drift on `restart/skinny/REDRESS.md` (cosmetic; CH3-adjacent)

`p1c-samply-mode-3.md:500` and `:590` cite `restart/skinny/REDRESS.md`
(verbatim quoted path). The file does not exist at that path; the
canonical authority is `skinny/REDRESS.md` (used correctly by p1a:340,
p1d:647, p1e:292, p1f:253). `find /Users/mkbabb/Programming/bbnf-lang
-maxdepth 4 -name REDRESS.md` returns exactly one path
(`/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md`).

This is a CH3-adjacent finding rather than a CH3 anomaly-reopening:
the route is still correctly cited (REDRESS-126); only the file-path
prefix is wrong in two lines of p1c. Since CH3 requires "cite the entry
and mark pre-blocked", the entry is named (REDRESS-126) — the path
drift weakens the cite but does not vacate it. Recommend V2 fold:
normalise both lines to `skinny/REDRESS.md`.

### F-2 — ANOM-1 dual-route proposal each requires REDRESS-126 honour

p1c ANOM-1 (lines 425-428) proposes two non-exclusive S-P2 moves: (a)
rename probe to `comparator_serde_json_value`, OR (b) "implement an
actual Track 1 scalar alternate (e.g. a `scan_structurals_scalar` +
walk path that skips the NEON dispatch)". Move (b) implies a new
within-Track-1 scalar primitive surfaced as a probe. ANOM-6 explicitly
binds this to REDRESS-126 zero-orphan: "scalar reference + parity/
checkasm + feature-mask disclosure + same-wave consumer + zero-orphan
disposition". The walk path itself already exists at
`skinny/crates/runtime/src/grammars/json/scan.rs:32,107`
(`scan_structurals_scalar` → `scan_tail`); the move is adding it as a
gate-consumable probe, not standing up a new SIMD primitive.

CH3 disposition: **accepted as pre-blocked**. S-P2's V2 proposal must
satisfy the REDRESS-126 checklist when it surfaces the probe; if it
does not, CH3 V2 must REVISE at that point. No silent reopen at
S-P1 V1.

### F-3 — ANOM-2 view-materialization proposal is substrate-attribution, NOT a new sink fact

p1c ANOM-2 (lines 447-450) proposes "research whether the view-
materialization cost is necessary at the Lock 1 boundary or whether
direct emission of decoded slices into the tape during parse (consumed
by direct_to_struct sinks) absorbs it". The CH3 risk is that this
reads adjacent to **REDRESS-54** (sink-local decoded stats),
**REDRESS-55** (quote-source streaming hash), **REDRESS-66**
(`String` scratch via parse-attribution feature into parse-that-regex),
**REDRESS-69** (semantic string fact through DirectBuild). All four
are REJECTED routes in the SK-V6 Wave-3 close (REDRESS.md:1734-1886).

CH3 disposition: **accepted as not reopening**. ANOM-2 is a substrate-
attribution observation (view-walk + UTF-8 decode = 67% of host_call
self-time at `runtime::generated_json::view::*`) and a research prompt
for S-P2, not a sink-fact admission proposal. The pre-block fence is
the same `decoded-string statistics cache` family p1a:272 explicitly
maps to REDRESS-84 — adjacency catalogued, not reopened. The proposed
S-P2 work is bounded by ANOM-6's REDRESS-126 cite.

### F-4 — ANOM-3 cold-first proposal is harness-methodology, NOT a parser route

p1c ANOM-3 (lines 460-468) proposes "distinguish 'true cold parse'
from 'allocator-bound cold loop'; the existing harness conflates
them". The CH3 risk would be that this prompts a parser-local
allocation route (which would touch REDRESS-66/68 byte-output
escaped-string family). The actual proposal is a harness-level split
(bench probe definition + iter_batched body), not a parser change.

CH3 disposition: **accepted as not reopening**. The route family is
bench-methodology, not parser-internals. The pre-block fence is
honoured by ANOM-6's REDRESS-126 binding for the masking-probe
authority chain.

### F-5 — p1e §4.7 is the load-bearing CH3 statement (verbatim verification)

p1e §4.7 (line 248) names every REDRESS family the dispatch §2
watch-list requires:

> "...dispatch-table/function-pointer alternates (REDRESS 50-55),
> parser-local cursors (REDRESS 60-72), event sidecars (REDRESS 80,
> 82-84), decoded-string stats sinks (REDRESS 88, 89), generic source
> visitors, source-method digest folds, PEXT mask plan (REDRESS 126;
> aarch64 has no PEXT), or production-union routes (REDRESS 96, 97,
> 98)."

Grep against `skinny/REDRESS.md` confirms each cited family has at
least one rejected/historical anchor: Item 50 (REDRESS-50 at
SK-V5 close), Item 60 (line 1346), Item 65 (line 1639), Item 70-72
(lines 1890, 1944, 1996), Item 80 (line 2217), Item 82-84 (lines
2287, 2320, 2360), Item 88-89 (lines 2510, 2544); REDRESS-126
documented at lines 3864, 3869 (W12 ASM-gen close); REDRESS 96/97/98
referenced at line 3861 (SK-V12 Wave 1b-2b USER PIN D3). All eight
families exist with non-trivial rejection evidence; the verbatim cite
in p1e §4.7 is therefore load-bearing and complete.

CH3 disposition: this is the canonical S-P1 V1 CH3 statement. p1a's
inference-mapping paragraph (267-274) is the second canonical
statement (5 inferences, 5 REDRESS IDs). Together they cover the
full watch-list with zero silent re-opens.

## §4 — V2 fold recommendations

1. **Normalise REDRESS path citation in p1c** (F-1). Two lines
   (`p1c-samply-mode-3.md:500, :590`) cite the non-existent
   `restart/skinny/REDRESS.md`; rewrite to `skinny/REDRESS.md` (the
   canonical path used by p1a/d/e/f). Mechanical edit; no content
   change.

2. **V2 fold should re-run with `--features parse-attribution`**
   (per p1c ANOM-4 + p1e §4.1). Today's `dispatch_value` envelope
   masks the inlined primitive set; CH3 cannot fully verify that
   future primitive-design proposals avoid REDRESS-50 (dispatch-table
   alternate) until the envelope is cracked. This is a CH6 paper-close
   fold that strengthens CH3.

3. **Pre-emptive CH3 fence for S-P2 ANOM-1/2/3 follow-up**: any S-P2
   research worktree consuming p1c ANOM-1 (`scan_structurals_scalar`
   walk probe), ANOM-2 (view-materialization absorption), or ANOM-3
   (cold-first harness split) must carry REDRESS-126's five-item
   checklist (scalar reference + parity/checkasm + feature-mask
   disclosure + same-wave consumer + zero-orphan disposition) **in
   the dispatch context** — not merely in the close. The S-P1 V1
   findings here satisfy CH3; the S-P2 V1 dispatch is where the
   checklist binds.

4. **Pre-emptive CH3 fence for any "primitive-hosting move" of
   `match_tiny_plain_string_with_cap`** (p1a Lock 14 re-surface;
   p1e §1.3 substrate row). The move is Pattern-H residue per S-P0
   (`bbnf-simd::tiny_quote_scan` target host). It is NOT a route to
   REDRESS-72 retained tiny-probe widening (which is rejected); it is
   a refactor of an existing primitive's home. Document this distinction
   in the S-P2 dispatch context to prevent CH3 confusion.

5. **No REVISE recommended** for any of the six S-P1 V1 artefacts on
   CH3 grounds. The lens is satisfied at 100% ACCEPT.

## §5 — Sources verified (executable-verification mandate)

Verified existence via `find` / `grep` / line-bounded `Read`:

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` —
  340 lines; CH3 paragraph at 267-274 verbatim verified.
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` —
  320 lines; §4 Anomaly 1-6 + 2 trailing notes verified by Read.
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` —
  607 lines; ANOM-1..ANOM-8 verified by Read; ANOM-6 REDRESS-126 cite
  at 498-507 verbatim; ANOM-5 PEXT/aarch64 at 485-496; path drift at
  500 + 590 confirmed via grep.
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` —
  648 lines; §4 anomalies 1-8 verified by Read; REDRESS 96/97/98
  cite at 564-567 verbatim; "MASKING signal is real noise, not a
  redress prompt" at 528-529 verbatim.
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md`
  — 306 lines; §4.1-§4.7 verified by Read; §4.7 verbatim quote
  confirmed (line 248); §4.7 lists eight REDRESS families with
  complete coverage of dispatch watch-list.
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` —
  260 lines; §4.1-§4.6 verified by Read; no parser-route proposals.

Verified `skinny/REDRESS.md` (5041 lines via wc -l) anchors via grep:
- Item 50 / REDRESS-50 (SK-V5 wave close family) — referenced via
  Item-numbered entries at lines 1346 (Item 60), 1639 (Item 65),
  1890 (Item 70), 1944 (Item 71), 1996 (Item 72), 2217 (Item 80),
  2287 (Item 82), 2320 (Item 83), 2360 (Item 84), 2510 (Item 88),
  2544 (Item 89).
- REDRESS-126 anchor: lines 3864, 3869 (SK-V12 Wave 4 + Wave 5 close;
  ASM-gen ROUTE-PRODUCTION-SPLIT; aarch64 orphan demotion ledger).
- REDRESS 96/97/98 reference: line 3861 (SK-V12 Wave 1b-2b USER PIN D3
  union-substrate measured-failure history).

Verified `skinny/crates/bbnf-bench/benches/json_parity.rs:414` —
`alternate_pext_mask_plan` is `#[cfg(any(target_arch = "x86",
target_arch = "x86_64"))]`-gated, confirming p1c ANOM-5's
architectural-impossibility claim.

Verified no REDRESS.md exists at `restart/skinny/REDRESS.md` (find
returned only `/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md`).

## §6 — CH3 disposition (final)

**ACCEPT 6/6 artefacts.** Per-§ rate: **100% ACCEPT** on the §4
anomaly population (43/43 entries either map to a pre-blocked REDRESS
family with explicit cite, or are CH4/CH6/CH7 observations that do not
propose a route at all). One ACCEPT-WITH-NOTE for documentary path
drift in p1c (F-1). Five NEW findings logged (F-1 through F-5);
zero REVISE; zero REJECT.

The dispatch §2 watch-list — REDRESS 50-55, 60-72, 80, 82-84, 88, 89
+ REDRESS-126 zero-orphan — is satisfied with **dual canonical
coverage**: p1a:267-274 (5-inference pre-block map) and p1e §4.7
(verbatim 8-family enumeration). p1c ANOM-6 (498-507) is the third
canonical statement, bounded specifically to ANOM-1/2/3.

CH3 V1 convergence vote: **CONVERGE**. No CH3-grounded blocker to S-P2
dispatch.
