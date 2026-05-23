# S-P1 V2 CHALLENGE — CH3 REGRESSION (REDRESS) Lens

V2 confirming pass over commit `069ba203c` (six-file atomic; "V2 light
micro-redispatch — five orphan REVISEs landed"). Authored 2026-05-23
post-V1 aggregator (`a3dfcaf38`). Six P1 axis files re-read at V2 line
counts: p1a (343 lines, +3), p1b (323, +3), p1c (616, +9), p1d (669, +21),
p1e (321, +15), p1f (269, +9). Authorities re-confirmed:
`restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH3,
`restart/prompts/ORCHESTRATOR.md` §3W (CH3 universal definition) + §3Z,
`restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md`,
`restart/skinny/tranches/sk-v14/SYNTHESIS.md` §0.4 P-1..P-7 pre-blocks,
`restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
(V1 verdict NOT-CONVERGED-V2-REQUIRED; CH3 forecast 100% hold post-fold),
`skinny/REDRESS.md` (5041 lines; REDRESS-126 anchors confirmed at
:3768 + :3864 + :3869), and V1 lens `CH3.md` (290 lines; the prior
canonical statement this V2 reads against).

V2 micro-fold scope binding (per dispatch): P1-C F-1 REDRESS path
normalisation (`restart/skinny/REDRESS.md` → `skinny/REDRESS.md` on the
two cites; V1 logged them as p1c:500 + :590; post-V2 they re-anchor at
:509 + :599 because the V2 build_flags_regime block at the head of
p1c added 9 lines). CH3 V2 must:

1. confirm F-1 closed (no `restart/skinny/REDRESS.md` cites remain in p1c
   or any of the other five artefacts);
2. confirm ANOM-1/2/3 + REDRESS-126 pre-block guard remains intact at the
   shifted line numbers;
3. confirm the V2 micro-fold introduced no new redress-adjacent route
   that silently re-opens the dispatch §2 watch-list
   (REDRESS 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 126);
4. fresh-finding scan over the V2 diff (86 insertions, 26 deletions).

## §1 — Disposition summary (V2 confirming pass)

| Axis | V1 disposition | V2 fold scope | V2 disposition | ACCEPT? |
|---|---|---|---|---|
| p1a-samply-mode-1 | ACCEPT (no CH3 changes) | F-V2-METHODOLOGY-1 (build_flags_regime row CORRECTED; native-CPU NOT pinned) + F-V2-P1A-MOVEMASK (12 line-anchor cites refreshed) | ACCEPT | YES |
| p1b-samply-mode-2 | ACCEPT (no pre-blocked route invoked) | F-V2-METHODOLOGY-1 (build_flags_regime: RUSTFLAGS unset disclosure) | ACCEPT | YES |
| p1c-samply-mode-3 | ACCEPT-WITH-NOTE (F-1 path drift on :500 + :590) | F-V2-METHODOLOGY-1 (build_flags_regime block at head adds 9 lines, shifts every later anchor +9) + F-V2-P1C-LINEDRIFT (3 NEON `:N` cites refreshed) + **F-1 closure** (`restart/skinny/REDRESS.md` → `skinny/REDRESS.md` on the two CH3 cites, now :509 + :599) | **ACCEPT** (note cleared) | YES |
| p1d-pmu-cycles | ACCEPT (REDRESS 96/97/98 pre-block cite intact) | F-V2-METHODOLOGY-1 (build_flags_regime + P1-A/B regime tag) | ACCEPT | YES |
| p1e-hot-leaf-attribution | ACCEPT (§4.7 load-bearing CH3 watch-list intact) | F-V2-P1E-1 (typed-plane file:line refresh on `generated_real_typed.rs`; symbol identities unchanged) + F-V2-METHODOLOGY-1 | ACCEPT | YES |
| p1f-results-delta | ACCEPT (documentary; no route proposed) | F-V2-P1F-1 (`track2_entry_point` column reclassified as contracted-deferral to C-2; CH5 reclassification, not CH3) | ACCEPT | YES |

**Per-lens V2 ACCEPT rate: 6/6 (100%).** V1 ACCEPT-WITH-NOTE on p1c
**cleared by F-1 closure**. Zero ANOMs reopen a pre-blocked REDRESS
family. Zero REVISEs. Zero REJECTs.

## §2 — F-1 closure verification (executable)

Dispatch micro-fold assertion: "P1-C F-1 REDRESS path normalized
(skinny/REDRESS.md at :509 + :599)". Verified by `grep -nE
'restart/skinny/REDRESS\.md|skinny/REDRESS\.md'` against the V2 file:

```
509:Per `skinny/REDRESS.md` REDRESS-126, any masking signal that
599:- `skinny/REDRESS.md` REDRESS-126 (zero-orphan guard; applies to ANOM-1/2/3)
```

Both cites now point to the canonical path. The +9-line drift between V1
anchors (:500 + :590) and V2 anchors (:509 + :599) is explained by the
build_flags_regime disclosure block inserted at the head of p1c (V2 diff
adds 9 lines at the `Build flags` block before line ~17, which propagates
+9 to every later anchor). The V1 hardening doc forecast this exact
post-fold geometry (`HARDENING-S-P1-V1-CONSOLIDATED.md:312-315`).

Cross-artefact `grep -rnE 'restart/skinny/REDRESS\.md'` against
`restart/skinny/tranches/sk-v14/research/p1/`:

- 0 hits in any of the six axis artefacts (the wrong-path string no
  longer appears in p1a/b/c/d/e/f).
- 4 hits in the V1 hardening cohort (V1 CH3.md + V1 consolidated) —
  these are correct historical records of the V1 finding, not new cites.

Git diff confirmation (commit `069ba203c` vs `069ba203c^`):

```
- Per `restart/skinny/REDRESS.md` REDRESS-126, any masking signal that
+ Per `skinny/REDRESS.md` REDRESS-126, any masking signal that
- - `restart/skinny/REDRESS.md` REDRESS-126 (zero-orphan guard; applies to ANOM-1/2/3)
+ - `skinny/REDRESS.md` REDRESS-126 (zero-orphan guard; applies to ANOM-1/2/3)
```

Exactly two `restart/skinny/REDRESS.md` → `skinny/REDRESS.md` edits in
the entire V2 commit. F-1 mechanically closed. **V1 ACCEPT-WITH-NOTE
cleared.**

## §3 — ANOM-1/2/3 + REDRESS-126 pre-block guard verification

The V1 lens canonized three statements as load-bearing for CH3:

1. **p1a:267-274** — 5-inference pre-block map (REDRESS-50, 51, 60, 83, 84).
2. **p1c ANOM-6** (now :507-516 post-shift) — REDRESS-126 zero-orphan
   applied verbatim to ANOM-1/2/3.
3. **p1e §4.7** (now :261-263 post-shift) — full 8-family watch-list
   enumeration (REDRESS 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 126).

V2 confirming reads (verbatim from V2 file content):

**p1c ANOM-6 (:507-516):**

> "Per `skinny/REDRESS.md` REDRESS-126, any masking signal that
> implies a new SIMD primitive needs scalar reference + parity/checkasm
> + feature-mask disclosure + same-wave consumer + zero-orphan
> disposition. ANOM-1's "implement actual Track 1 scalar alternate" and
> ANOM-2's "absorb view materialization into parse-time emission" both
> require S-P2 to honor REDRESS-126; they are NOT routes to a new SIMD
> primitive without those gates. Flagged so CH3 (REGRESSION) catches any
> V2 fold that silently re-opens REDRESS 50-55, 60-72, 80, 82-84, 88, 89."

Status: intact, identical wording, only the path prefix corrected.

**p1c ANOM-5 (:494-505)** — PEXT/aarch64 architectural-impossibility
finding (sk-v13 P1-C V2 restated): intact. NEON `classify_tbl4` +
`escape_mask_64` + `prefix_xor_64` cite at `scan.rs:200-267` intact.

**p1c ANOM-8 (:532-550)** — structural-SIMD-speedup corpus-correlated;
explicit "does NOT re-open REDRESS 96/97/98" intact.

**p1e §4.7 (:261-263)** verbatim:

> "...dispatch-table/function-pointer alternates (REDRESS 50-55),
> parser-local cursors (REDRESS 60-72), event sidecars (REDRESS 80,
> 82-84), decoded-string stats sinks (REDRESS 88, 89), generic source
> visitors, source-method digest folds, PEXT mask plan (REDRESS 126;
> aarch64 has no PEXT), or production-union routes (REDRESS 96, 97, 98)."

Status: intact, identical wording, identical 8-family enumeration.

**p1d Anomaly 6 (cite at :585-588 post-shift)** — REDRESS 96/97/98
"pre-blocked against any union substrate route" intact.

**p1a CH3 inference paragraph (post-shift)** — 5-inference REDRESS pre-
block (REDRESS-50, 51, 60, 83, 84) intact.

**REDRESS-126 anchors in canonical `skinny/REDRESS.md`** verified by
grep: lines 3768 (`Item 126 closes W4 under G-W4-ASM-GEN-CONSUMER`),
3864 (`ASM-gen disposition: W4 is recorded by REDRESS-126 as a measured`),
3869 (`Final aarch64 orphan state is zero. REDRESS-126 demotes`). All
three V1-cited anchors still resolve.

CH3 watch-list disposition unchanged: zero anomalies in any of the six
axis files implicitly reopen REDRESS 50-55 / 60-72 / 80 / 82-84 / 88,
89 / 96-98 / 126. All adjacencies carry explicit cite + pre-block flag.

## §4 — V2-diff fresh-finding scan

The V2 commit (`069ba203c`) adds 86 lines, removes 26. The full added
text was scanned with `grep -E '^\+'` filtered against keywords:
`REDRESS|cursor|sidecar|sink|tape|dispatch.table|StringBlock16|union|
substrate|prefix|pext|CTZ|mantissa|classifier`. Hits in additions:

| File | Added line | Adjacency | Pre-blocked family? | New route proposed? |
|---|---|---|---|---|
| p1c (head, ~:17-25) | `build_flags_regime: RUSTFLAGS="-C target-cpu=native"` block | CH4 disclosure | NO | NO |
| p1c :509 | `skinny/REDRESS.md` path normalisation | F-1 close | YES (REDRESS-126; already cited) | NO |
| p1c :599 | `skinny/REDRESS.md` path normalisation | F-1 close | YES (REDRESS-126; already cited) | NO |
| p1a, p1b, p1d build_flags_regime additions | RUSTFLAGS regime disclosure | CH4 cross-regime guard | NO | NO |
| p1a NEON cite refreshes (`bulk_emit_positions_64.rs:2`, `bitmap_prefix_xor_64.rs:2`, `eob_pad_clamp.rs:4` + `fn` signature + `#[inline]` attribute notes) | line-anchor truth-up | CH1/CH2 grammar-neutral cite | NO | NO |
| p1c NEON cite refreshes (same primitives) | line-anchor truth-up | CH1/CH2 cite | NO | NO |
| p1e typed-plane refresh (`generated_real_typed.rs` line-drift refresh; symbol identities unchanged per V2 §15-note) | CH1 V1 REVISE close | NO | NO | NO |
| p1f `track2_entry_point` contracted-deferral framing | CH5 reclassification | NO | NO | NO |

**Verdict:** Zero V2 additions propose a parser primitive, a new sink
fact, a new dispatch-table alternate, a new parser-local cursor, a new
event sidecar, a new decoded-string stats route, a PEXT alternate, or
a production-union substrate. Every V2 addition is one of: (a) CH4
methodology disclosure (RUSTFLAGS regime), (b) CH1/CH2 line-anchor
truth-up (no symbol identity changes), (c) CH5 reclassification (column
schema deferral, not a parser change), (d) F-1 cosmetic path normalise.

The V2 micro-fold is therefore **CH3-inert by construction** — exactly
as the V1 aggregator forecast (`HARDENING-S-P1-V1-CONSOLIDATED.md:417`:
"CH3 100% → 100%, F-1 path normalisation in F-V2-P1C-LINEDRIFT clears
ACCEPT-WITH-NOTE").

## §5 — Fresh findings (V2 lens)

### F-V2-CH3-1 — V1 CH3.md cite shorthand records pre-shift anchors

The V1 lens `CH3.md` cites p1c F-1 at `:500` and `:590` (V1 line
geometry); the V2 micro-fold landed the normalisation at `:509` and
`:599` (V2 line geometry, +9 from the build_flags_regime head insert).
The V1 CH3 record is therefore correct **at V1 line geometry**, not
incorrect — but a downstream reader reconciling against V2 will need to
add +9 to find the anchors. **Disposition:** documentary; no V2 action
required (the V1 hardening doc itself notes this geometry shift, and
the V2 dispatch context already binds the new :509 + :599 anchors).

### F-V2-CH3-2 — V1 lens F-5 cite-anchor numbers stable

The V1 lens F-5 verification table cites REDRESS.md anchor lines at
:3864 + :3869 (REDRESS-126) and :3861 (REDRESS 96/97/98 reference) +
Item 50/60/65/70/72/80/82/83/84/88/89 at lines 1346/1639/1890/1944/
1996/2217/2287/2320/2360/2510/2544. V2 fresh grep confirms REDRESS-126
anchors (:3768, :3864, :3869) — line :3768 is a new anchor V1 missed
but does not change the disposition (the REDRESS-126 family resolves
to non-trivial rejected-route evidence either way). **Disposition:**
documentary enrichment; CH3 V1 F-5 verdict (load-bearing canonical
statement) still holds at V2.

### F-V2-CH3-3 — No new ANOMs in V2 commit

The V2 commit `069ba203c` does not add any §4 anomalies to any of the
six artefacts; it only refreshes existing rows + adds methodology
disclosure + closes F-1. The §4 anomaly population (43 entries) at V1
remains 43 entries at V2. CH3 V2 is therefore a **strict superset**
confirmation of the V1 lens, not a re-evaluation. **Disposition:** V1
lens canon binds; V2 confirms.

### F-V2-CH3-4 — `cold_first_parse` regression on five corpora (p1c §3.1)

Re-reading p1c §3.1 at V2 geometry, the SK-V13 → SK-V14 delta table on
`cold_first_parse` shows regressions on five view-materialization-heavy
corpora (correlated with ANOM-1). The V2 commit did not touch this
section. CH3 verification: the regressions are flagged as
"correlate with view-materialization-heavy corpora" (substrate
attribution, NOT a new parser route). ANOM-6 already binds any S-P2
follow-up to REDRESS-126's checklist; the §3.1 regressions therefore
inherit the same pre-block fence. **Disposition:** no new CH3 finding;
ANOM-6 binding sufficient.

### F-V2-CH3-5 — REDRESS 96/97/98 reference shifted in p1d

p1d Anomaly 6 cite (V1 :564-567 → V2 ~:585-588 due to build_flags_regime
+21 line addition) restates "MASKING signal is real noise, not a redress
prompt" and "pre-blocked against any union substrate route". Re-verified
at V2 geometry: text intact, identical wording, REDRESS 96/97/98 cite
identical. **Disposition:** intact; geometry-only shift.

## §6 — V3 fold recommendations

**None CH3-grounded.** The V1 CH3 V2-fold recommendations 1-5 are now
disposed:

1. F-1 path normalisation — **CLOSED** in V2 (F-V2-P1C-LINEDRIFT cohort).
2. parse-attribution rebuild — **DEFERRED to S-P2** per V1 aggregator
   Option X (primitive-design ground-truth, not lens-correctness fix).
   CH3 does not block this deferral; ANOM-4 dispatch_value folded
   symbol stays AUDIT-PENDING for S-P2 design.
3. Pre-emptive CH3 fence for S-P2 ANOM-1/2/3 follow-up — **BINDING for
   S-P2 dispatch context**, not a V2/V3 P1 action. ANOM-6 + p1e §4.7
   discharge the S-P1 CH3 obligation.
4. Pre-emptive CH3 fence for `match_tiny_plain_string_with_cap`
   primitive-hosting move — **BINDING for S-P2 dispatch context**.
   p1a's V2 movemask annotation (F-V2-P1A-MOVEMASK; `generated.rs:160,
   176`) refines the cite anchor; the Pattern-H residue distinction
   from REDRESS-72 remains a S-P2 framing concern.
5. No REVISE recommended — **HOLDS** at V2 (still no CH3 REVISE).

**V3 forecast for CH3:** 100% ACCEPT with zero changes expected. The
lens is closed; only an architectural escalation (e.g. S-P2 design
proposing a new substrate primitive without REDRESS-126 honour) could
re-open it, and that is out of S-P1 V3 scope.

## §7 — Sources verified (executable-verification mandate)

Verified existence + content at V2 geometry via `find` / `grep` /
line-bounded `Read`:

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` —
  343 lines (V1 was 340; +3 from F-V2-METHODOLOGY-1 build_flags_regime
  block). CH3 inference paragraph intact.
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` —
  323 lines (V1: 320; +3). CH3 disposition unchanged.
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` —
  616 lines (V1: 607; +9). ANOM-6 REDRESS-126 cite at :509-516 verbatim
  verified. F-1 normalisation at :509 + :599 verified by grep
  (zero `restart/skinny/REDRESS.md` hits remain).
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` —
  669 lines (V1: 648; +21). REDRESS 96/97/98 cite intact at shifted
  anchor.
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md`
  — 321 lines (V1: 306; +15). §4.7 verbatim 8-family enumeration intact
  at :261-263.
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` —
  269 lines (V1: 260; +9). No parser-route proposals introduced.

Verified `skinny/REDRESS.md` REDRESS-126 anchors via grep:
`:3768` (`Item 126 closes W4 under G-W4-ASM-GEN-CONSUMER`),
`:3864` (`W4 is recorded by REDRESS-126 as a measured`),
`:3869` (`Final aarch64 orphan state is zero. REDRESS-126 demotes`).
All three intact.

Verified git diff `069ba203c^..069ba203c` against the six artefacts:
exactly **two** `restart/skinny/REDRESS.md → skinny/REDRESS.md` edits,
both in p1c at the post-shift anchors. Zero REDRESS-family terms
introduced in the +86 line additions (only cosmetic path correction +
CH4 methodology disclosure + line-anchor truth-up).

Verified `find /Users/mkbabb/Programming/bbnf-lang -maxdepth 4 -name
REDRESS.md` returns exactly one path
(`/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md`) — F-1 cited
path `restart/skinny/REDRESS.md` does not and never has existed.

Verified V1 hardening forecast intact: `HARDENING-S-P1-V1-CONSOLIDATED.md:417`
("CH3 100% → 100%, F-1 path normalisation in F-V2-P1C-LINEDRIFT clears
ACCEPT-WITH-NOTE") matches the V2 outcome exactly.

## §8 — CH3 V2 disposition (final)

**ACCEPT 6/6 artefacts. V2 ACCEPT-rate: 100% (43/43 §4 entries; zero
silent re-opens; zero ANOM REVISE; zero ANOM REJECT).**

The V1 ACCEPT-WITH-NOTE on p1c (F-1 documentary path drift) is
**CLEARED** by F-V2-P1C-LINEDRIFT. The V1 dual-canonical CH3 statement
(p1a inference map + p1e §4.7 8-family enumeration + p1c ANOM-6
ANOM-1/2/3 binding) survives the V2 micro-fold unchanged in substance
and corrected in cite path. Five new V2-lens findings logged
(F-V2-CH3-1 through F-V2-CH3-5); none blocking; four are pure
documentation/geometry observations and one re-confirms an intact
section.

The dispatch §2 watch-list — REDRESS 50-55, 60-72, 80, 82-84, 88, 89,
96-98, 126 — is satisfied at V2 with the same triple-canonical coverage
plus corrected path on the third statement.

**CH3 V2 convergence vote: CONVERGE.** No CH3-grounded blocker to
either V3 confirming pass (which would be a strict-superset re-read
yielding the same verdict) or to S-P2 dispatch. The lens is closed in
substance at V2; V3 carries no CH3 work.
