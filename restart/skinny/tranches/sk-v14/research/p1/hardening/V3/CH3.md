# S-P1 V3 CHALLENGE — CH3 REGRESSION (REDRESS) Lens

V3 pure-confirming pass authored 2026-05-23 over the unchanged V2 P1
artefact set. The six P1 axis files have **no new commits** since the
V2 micro-fold landed in `069ba203c`; `git log --oneline -- p1{a..f}*.md`
returns exactly two commits (`3510c1de5` V1 atomic; `069ba203c` V2
micro-fold), and the subsequent commit `4ad8f1949`
(`docs(sk-v14-p1-hardening-V2): challenge V2 + consolidated`) touched
only the V2 hardening cohort, **not** the artefacts under review. V3
is therefore by construction a strict-superset re-read of the V2 CH3
lens against bit-identical inputs.

Authorities re-confirmed end-to-end:
`restart/prompts/skinny/PASS-1-PROFILE.md` §3 (CH3 specialisation),
`restart/prompts/ORCHESTRATOR.md` §3W (CH3 universal definition) + §3Z
(convergence rule),
`restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md`,
`restart/skinny/tranches/sk-v14/SYNTHESIS.md` §0.4 P-1..P-7 pre-blocks,
`restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md`
(V1 dispatch context — V3 inherits via V2 chain; no V3-specific
dispatch context file exists),
`restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
(V1 verdict NOT-CONVERGED-V2-REQUIRED),
`restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
(V2 verdict: sub-axis-weighted 174/175 = 99.43 %; CH3 ACCEPT 6/6 =
100 %),
`skinny/REDRESS.md` (REDRESS-126 anchors at :3768 + :3864 + :3869
re-grep-confirmed at V3 read), V1 lens `CH3.md` (289 lines), and V2
lens `CH3.md` (320 lines — the prior canonical statement this V3 reads
against).

V3 mandate (per dispatch): "V3 confirms baseline holds." Concretely:

1. confirm F-1 closure still holds (`restart/skinny/REDRESS.md` does
   not re-appear anywhere in the six artefacts);
2. confirm ANOM-1/2/3 + REDRESS-126 pre-block guard remains intact at
   the V2 line anchors (no further drift);
3. confirm zero new REDRESS-adjacent routes introduced since V2;
4. fresh-finding scan over the (empty) V2→V3 artefact diff;
5. recompute the 43-anomaly §4 census;
6. recompute the REDRESS-126 anchor triple in canonical
   `skinny/REDRESS.md`.

## §1 — Disposition summary (V3 confirming pass)

| Axis | V1 | V2 | V3 fold scope | V3 disposition | ACCEPT? |
|---|---|---|---|---|---|
| p1a-samply-mode-1 (343 lines) | ACCEPT | ACCEPT (build_flags_regime; movemask cite refresh) | **none** (no commit since V2) | ACCEPT | YES |
| p1b-samply-mode-2 (323 lines) | ACCEPT | ACCEPT (build_flags_regime: RUSTFLAGS unset disclosure) | **none** | ACCEPT | YES |
| p1c-samply-mode-3 (616 lines) | ACCEPT-WITH-NOTE | ACCEPT (F-1 closed; build_flags_regime; NEON cite refresh) | **none** | ACCEPT | YES |
| p1d-pmu-cycles (669 lines) | ACCEPT | ACCEPT (build_flags_regime + regime tag) | **none** | ACCEPT | YES |
| p1e-hot-leaf-attribution (321 lines) | ACCEPT | ACCEPT (typed-plane file:line refresh) | **none** | ACCEPT | YES |
| p1f-results-delta (269 lines) | ACCEPT | ACCEPT (`track2_entry_point` CH5 reclassification) | **none** | ACCEPT | YES |

**Per-lens V3 ACCEPT rate: 6/6 (100 %).** Zero ANOMs reopen a
pre-blocked REDRESS family. Zero REVISEs. Zero REJECTs. The V1
ACCEPT-WITH-NOTE on p1c (cleared at V2) remains cleared at V3 by
construction (no new commits could re-introduce drift).

**§4 anomaly census (V3 recount):** 43/43 anomalies intact
(p1a CH3 inference paragraph: 1; p1b CH3 disposition: 0 §4 named
anomalies in scope; p1c ANOM-1..ANOM-8: 8; p1d Anomalies 1..7: 7;
p1e §4.1..§4.7 with sub-items: 19 across hot-leaf census; p1f sched
escalations 1..8: 8 — totals 43). Identical to V2.

## §2 — F-1 closure persistence verification (executable)

Re-grep against the six V3 artefacts for the wrong-path string:

```
grep -rnE 'restart/skinny/REDRESS\.md|skinny/REDRESS\.md' p1{a..f}*.md
```

Hits (V3 read):

- `p1a-samply-mode-1.md:343` — `skinny/REDRESS.md (route-guard registry)`
  (authority list; canonical path)
- `p1c-samply-mode-3.md:509` — `Per \`skinny/REDRESS.md\` REDRESS-126`
  (V2-corrected anchor; canonical path)
- `p1c-samply-mode-3.md:599` — `\`skinny/REDRESS.md\` REDRESS-126`
  (V2-corrected anchor; canonical path)
- `p1d-pmu-cycles.md:668` — `\`skinny/REDRESS.md\` (pre-blocked routes…)`
  (authority list; canonical path)
- `p1e-hot-leaf-attribution.md:307` — `\`skinny/REDRESS.md\`
  (rejected-route ledger; CH3 binding)` (authority list; canonical
  path)
- `p1f-results-delta.md:262` — `…skinny/REDRESS.md items 145-148…`
  (canonical path)

**Zero `restart/skinny/REDRESS.md` hits.** F-1 closure still holds at
V3. All six axis files cite the canonical path.

`find /Users/mkbabb/Programming/bbnf-lang -maxdepth 4 -name REDRESS.md`
returns exactly one path
(`/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md`); the wrong
path `restart/skinny/REDRESS.md` does not exist (V2 §7 finding
preserved at V3 by file-system invariance).

The two p1c F-1 cites remain at line geometry :509 + :599 (the V2
landing site); no further line drift has occurred because no commits
have touched p1c since `069ba203c`.

## §3 — ANOM-1/2/3 + REDRESS-126 pre-block guard re-verification

The three load-bearing CH3 canonical statements at V2 are re-read
verbatim at V3 line geometry:

**p1c ANOM-6 (:507-516)** — verbatim:

> "Per `skinny/REDRESS.md` REDRESS-126, any masking signal that
> implies a new SIMD primitive needs scalar reference + parity/checkasm
> + feature-mask disclosure + same-wave consumer + zero-orphan
> disposition. ANOM-1's "implement actual Track 1 scalar alternate" and
> ANOM-2's "absorb view materialization into parse-time emission" both
> require S-P2 to honor REDRESS-126; they are NOT routes to a new SIMD
> primitive without those gates. Flagged so CH3 (REGRESSION) catches any
> V2 fold that silently re-opens REDRESS 50-55, 60-72, 80, 82-84, 88, 89."

Status: identical at V3.

**p1e §4.7 (:261-263)** — verbatim:

> "…dispatch-table/function-pointer alternates (REDRESS 50-55),
> parser-local cursors (REDRESS 60-72), event sidecars (REDRESS 80,
> 82-84), decoded-string stats sinks (REDRESS 88, 89), generic source
> visitors, source-method digest folds, PEXT mask plan (REDRESS 126;
> aarch64 has no PEXT), or production-union routes (REDRESS 96, 97,
> 98)."

Status: identical at V3, identical 8-family enumeration.

**p1a 5-inference pre-block (:271-275)** — verbatim:

> "The five candidate inferences a careless reader might draw — (a)
> "dispatch table replaces match" → REDRESS-50; (b) "parser-local
> cursor instead of state" → REDRESS-51; (c) "event-sidecar tape"
> → REDRESS-60; (d) "source-method digest for unicode" → REDRESS-83;
> (e) "decoded-string statistics cache" → REDRESS-84 — are all…"

Status: identical at V3.

**p1d Anomaly 6 (:581-588)** — REDRESS 96/97/98 "remain pre-blocked
against any union substrate route" intact, identical at V3.

**p1c ANOM-5 (:494-505)** — PEXT/aarch64 architectural-impossibility:
intact at V3.

**p1c ANOM-8 (:532-550)** — structural-SIMD-speedup corpus-correlated;
explicit "does NOT re-open REDRESS 96/97/98" intact at V3.

**REDRESS-126 anchors in canonical `skinny/REDRESS.md`** recomputed
via grep at V3:

```
3768:- Item 126 closes W4 under `G-W4-ASM-GEN-CONSUMER` as
3864:- ASM-gen disposition: W4 is recorded by REDRESS-126 as a measured
3869:- Final aarch64 orphan state is zero. REDRESS-126 demotes
```

Identical to V2 §3 recount; identical to V2 §7 verification; all three
anchors still resolve.

**CH3 watch-list disposition unchanged at V3:** zero anomalies in any
of the six axis files implicitly reopen REDRESS 50-55 / 60-72 / 80 /
82-84 / 88, 89 / 96-98 / 126. All adjacencies carry explicit cite +
pre-block flag.

## §4 — V3 fresh-finding scan (against null V2→V3 artefact diff)

The V2→V3 diff over the six P1 artefacts is **empty**:

```
git diff 069ba203c..HEAD -- restart/skinny/tranches/sk-v14/research/p1/p1{a..f}*.md
(returns no output)
```

Therefore zero V3 additions exist to scan for parser-primitive
proposals, new sink facts, new dispatch-table alternates, new
parser-local cursors, new event sidecars, new decoded-string stats
routes, PEXT alternates, or production-union substrates. The V2
fresh-finding scan table (V2 §4) carries forward verbatim with all
"NO" verdicts in the "New route proposed?" column.

**Verdict:** the V3 confirming pass is CH3-inert by construction —
exactly as the V2 lens forecast in its §6 ("V3 forecast for CH3: 100 %
ACCEPT with zero changes expected. The lens is closed; only an
architectural escalation could re-open it, and that is out of S-P1 V3
scope.").

## §5 — Fresh findings (V3 lens)

### F-V3-CH3-1 — No new findings; V2 canon binds

The V2 lens authored five V3-relevant findings:

- F-V2-CH3-1 (V1 CH3.md cite shorthand records pre-shift anchors) —
  documentary; carried forward unchanged at V3; the V1→V2 +9-line
  shift remains the same +9 at V3 (no further drift).
- F-V2-CH3-2 (V1 lens F-5 cite-anchor numbers stable) — V3 re-grep
  reconfirms the REDRESS-126 anchor triple (:3768, :3864, :3869);
  identical to V2.
- F-V2-CH3-3 (No new ANOMs in V2 commit) — V3 trivially extends: no
  new ANOMs in V3 either (zero commits between V2 and V3).
- F-V2-CH3-4 (`cold_first_parse` regression on five corpora) — text
  intact at V3 line geometry; ANOM-6 binding remains sufficient.
- F-V2-CH3-5 (REDRESS 96/97/98 reference in p1d) — re-verified at V3
  (:585 anchor); identical to V2.

V3 introduces **no new findings of its own**. The lens reads as a
mechanical re-confirmation; every observation reduces to "V2 verdict
preserved by file-invariance".

### F-V3-CH3-A — Cross-cycle invariance proof (mechanical)

The V2 CH3 file has 320 lines; V3 reads the same V2 artefacts at the
same line numbers (343 / 323 / 616 / 669 / 321 / 269). The
recomputation of every load-bearing anchor in §2 + §3 produces
byte-identical text. Per `feedback_clean_regen_discipline`, this is
the kind of mechanical re-derivation that does not warrant a fresh
finding — it warrants a confirming tick. Logged here only because the
dispatch asked for "any new finding".

### F-V3-CH3-B — V3 dispatch chain inherits via V2

There is no V3-specific `CHALLENGE-CONTEXT.md` under
`restart/skinny/tranches/sk-v14/research/p1/hardening/V3/`; V3 lens
agents read the V1 dispatch context end-to-end and the V2 lens output
as the prior canonical statement. This matches the §3Z convergence
discipline (≥95 % × 2 cycles, zero orphan REVISEs): V3 is the second
of two consecutive ≥95 % cycles for CH3 (V2 = 100 %, V3 = 100 %).
**Disposition:** documentary; flags successful inheritance of the
dispatch chain.

## §6 — V4 fold recommendations

**None.** The CH3 lens is closed in substance at V2 and re-confirmed
at V3. No V4 dispatch action recommended for CH3:

1. F-1 path normalisation — **CLOSED at V2**; persistent at V3.
2. parse-attribution rebuild — **DEFERRED to S-P2** (binding from V2;
   CH3 does not block; ANOM-4 dispatch_value folded symbol stays
   AUDIT-PENDING for S-P2 design).
3. Pre-emptive CH3 fence for S-P2 ANOM-1/2/3 follow-up — **BINDING
   for S-P2 dispatch context** (V2 §6.3); ANOM-6 + p1e §4.7 discharge
   the S-P1 CH3 obligation.
4. Pre-emptive CH3 fence for `match_tiny_plain_string_with_cap`
   primitive-hosting move — **BINDING for S-P2 dispatch context** (V2
   §6.4); p1a's V2 movemask annotation (F-V2-P1A-MOVEMASK;
   `generated.rs:160, 176`) refines the cite anchor.
5. No REVISE — **HOLDS at V3** (still no CH3 REVISE).

**V4 forecast for CH3:** absent any new commits to the P1 axis files,
V4 would yield identical 100 % ACCEPT. Only an architectural
escalation (e.g. S-P2 design proposing a new substrate primitive
without REDRESS-126 honour) could re-open the lens, and that is out
of S-P1 scope.

**Per §3Z (≥95 % × 2 cycles, zero orphan REVISEs):** CH3 satisfies
the convergence rule on V2 (100 %) + V3 (100 %); CH3 standalone
**CLOSES at V3** by the same construction that closed CH7 at V2 in
the V2 consolidated (which used V1=100 % + V2=100 % chain).

## §7 — Sources verified (executable-verification mandate)

Verified existence + content at V3 read via `find` / `grep` /
line-bounded `Read`:

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` —
  343 lines (matches V2). CH3 inference paragraph at :271-275
  verbatim intact; authority list citing `skinny/REDRESS.md` at :343.
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` —
  323 lines (matches V2). CH3 disposition unchanged; no §4 anomaly
  reopens any pre-blocked family.
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` —
  616 lines (matches V2). ANOM-6 REDRESS-126 cite at :509-516 verbatim
  re-verified. F-1 normalisation at :509 + :599 re-verified by grep
  (zero `restart/skinny/REDRESS.md` hits remain).
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` —
  669 lines (matches V2). REDRESS 96/97/98 pre-block cite at :585
  intact; authority list citing `skinny/REDRESS.md` at :668.
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md`
  — 321 lines (matches V2). §4.7 verbatim 8-family enumeration intact
  at :261-263; authority list citing `skinny/REDRESS.md` at :307.
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` —
  269 lines (matches V2). No parser-route proposals introduced; W13/W14
  REDRESS attribution at :262.

Verified `skinny/REDRESS.md` REDRESS-126 anchors via grep at V3:
`:3768`, `:3864`, `:3869` — identical to V2 §7.

Verified `git log --oneline -- restart/skinny/tranches/sk-v14/research/p1/p1*.md`
returns exactly two commits (`3510c1de5` V1 atomic, `069ba203c` V2
micro-fold); the V2 hardening commit `4ad8f1949` did not touch the
artefacts under review. V2→V3 artefact diff is empty.

Verified `find /Users/mkbabb/Programming/bbnf-lang -maxdepth 4 -name
REDRESS.md` returns exactly one path
(`/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md`); the wrong
path `restart/skinny/REDRESS.md` still does not exist.

Verified V2 hardening forecast intact:
`HARDENING-S-P1-V2-CONSOLIDATED.md` §0.1 CH3 row records "V2 rate
100 %, ACCEPT (F-1 path normalisation closed in F-V2-P1C-LINEDRIFT;
ANOM-1/2/3 + REDRESS-126 pre-block guard intact at shifted line
geometry; 5 V2-lens documentary findings, none blocking)" — matches
the V3 outcome by file-invariance.

## §8 — CH3 V3 disposition (final)

**ACCEPT 6/6 artefacts. V3 ACCEPT-rate: 100 % (43/43 §4 entries;
zero silent re-opens; zero ANOM REVISE; zero ANOM REJECT).**

The V2 ACCEPT 6/6 verdict is preserved by file-invariance: the six
P1 axis artefacts have not changed since commit `069ba203c`; the V3
read therefore returns byte-identical evidence on every load-bearing
canonical statement (p1a 5-inference map, p1c ANOM-6 REDRESS-126
binding, p1e §4.7 8-family enumeration, p1d Anomaly 6 REDRESS 96/97/98
pre-block, p1c ANOM-5 PEXT/aarch64 finding, p1c ANOM-8 SIMD-speedup
guard). Zero new V3 findings; two confirming-only documentary entries
(F-V3-CH3-A invariance proof; F-V3-CH3-B inheritance chain) logged
for completeness.

The dispatch §2 watch-list — REDRESS 50-55, 60-72, 80, 82-84, 88, 89,
96-98, 126 — remains satisfied at V3 with the same triple-canonical
coverage and corrected path established at V2.

**CH3 V3 convergence vote: CONVERGE.** Per §3Z, CH3 chain satisfies
≥95 % × 2 consecutive cycles (V2 = 100 %, V3 = 100 %) with zero
orphan REVISEs across the entire chain; CH3 standalone **CLOSES at
V3**. No CH3-grounded blocker to either V4 (which would yield
identical 100 % by file-invariance) or to S-P2 dispatch. The lens
carries no V4 work.
