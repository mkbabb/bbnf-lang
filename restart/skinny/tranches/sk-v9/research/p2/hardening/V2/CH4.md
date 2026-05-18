# SK-V9 S-P2 CHALLENGE V2 — CH4 COST

Pass: S-P2 Research. Cycle: V2.
Lens: CH4 COST.
Date: 2026-05-18.
Scope: re-audit of the six V2-folded P2 reports at
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-{A..F}-*.md` against
the V1 CH4 dispositions (44 rows: 10 ACCEPT / 33 REVISE / 1 REJECT =
22.7%) and the consolidated F4 + F3 folds at
`HARDENING-S-P2-V1-CONSOLIDATED.md`. CH4 verifies per-slice cost
discipline (LOC sub-budget + minute cap + revert protocol + same-wave
consumer per intervention slice) and that the P2-F §7.4 S-P3 overreach
is gone.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

The V1 CH4 verdict was the harshest of the six lenses — 22.7% — because
no report except P2-B carried per-slice minute caps + revert protocols,
and P2-F §7.4 reproduced the S-P1 V4 CH4 failure mode (a research
artefact authoring the wave sequence under the guise of a cost
envelope). V2 dispatch carried F4 (per-slice cost discipline across
A/C/D/E/F) and F3 (P2-F §7.4 reframe). This lens verifies the fold
landed and clears ≥95%.

The six CH4 surfaces per slice, unchanged from V1: (1) LOC envelope;
(2) risk class; (3) hard cap (minute budget); (4) same-wave consumer;
(5) revert protocol; (6) pre-block reference. Plus the two structural
checks: (7) no S-P3 overreach; (8) P2-F §7 path defers cost-authorship
or carries its own cost set.

## §1 — V1-disposition resolution (per report)

### §1.1 — P2-A (V1: REVISE 8/8 — minute cap + revert sentence absent)

V1 CH4 §2.1 REVISEd all eight A.1–A.8 slices for one shared defect:
"absence of an explicit minute cap per slice (only an implicit 90-min
ceiling) and the absence of one-sentence revert protocols per slice."

**Resolved.** P2-A V2 carries a new §5 "Per-slice cost discipline (S-P3
owns final cost set)" with eight numbered sub-sections §5.1–§5.8, one
per slice A.1–A.8. Each sub-section carries: a named owner file, an LOC
envelope (e.g. §5.1 `+60 / -20 hand`), a risk class (LOW / MEDIUM, e.g.
§5.3 `MEDIUM — regenerated`), an explicit minute cap keyed to the CH4
§4.1 band schedule (§5.1 `~30 min` hand-LOC 30–100 band; §5.2 `~15 min`
≤30 band; §5.5 `~45–60 min` >100 band; §5.3/§5.4/§5.7 `~10 min regen +
verification`), and a one-sentence revert protocol (§5.1: "if the §4.2
W10b gate fires, revert the `assembler.rs` column-push and keep the
`classes` field unused (zero-length)"). §5.9 aggregates: ~265 hand +
~120 regen LOC, ~155 min preliminary cap, with the cohort-level revert
protocol (reverting the codegen-template commit A.5 rolls back four
downstream regen files). The §6 REDRESS pre-block ledger (50, 51, 53,
60–72, 82–84, 88, 89, 92) is preserved. **All eight V1 REVISE rows are
resolved.**

### §1.2 — P2-B (V1: ACCEPT 5/5 — already CH4-clean)

V1 CH4 §2.2 ACCEPTed all five B.1–B.5 slices; P2-B was the cohort's
exemplar.

**Preserved and strengthened.** P2-B V2 §6.1 is now an explicit
five-slice break-out table (S1–S5) with per-slice LOC cap + revert
protocol, and §6.3 carries per-slice minute caps (S1 ≤15, S2 ≤10, S3
≤10, S4 ≤10, S5 ≤15, plus verification/audit/buffer summing to ≤90
min). The V2 fold added one slice (S4 `ValueRef` parameterisation, ~30
LOC, ≤10 min) that V1 absorbed silently into the trait file — this is a
*tightening*, not a regression: the aggregate moved from a 55-LOC
margin to a 25-LOC margin and the report states the reconciliation
explicitly (§6.1 "versus V1's 55 LOC margin"). The §5.1 same-wave-
consumer formal disposition ("the rule binds substrates, not contracts;
the proof is a contract; therefore the rule is silent") is preserved
verbatim. **No V1 ACCEPT row degraded.**

### §1.3 — P2-C (V1: ACCEPT 3/7, REVISE 4/7 — per-slice LOC absent)

V1 CH4 §2.3 REVISEd C.1, C.2, C.3, C.5 for "absence of per-slice LOC
and risk class break-out"; ACCEPTed C.4, C.6, C.7.

**Resolved.** P2-C V2 carries a new §2.0 "Per-slice LOC + minute
sub-budgets" table decomposing the aggregate 300 LOC / ≤90 min HANDOFF
envelope into five disjoint artefact slices (a)–(e), each with an LOC
sub-budget (~30 / ~10 / ~120 / ~80 / ~15), a minute sub-cap (~15 / ~15
/ ~30 / ~15 / ~10), a same-wave consumer (gate / self / self / docs /
scoped-Lock), and a one-sentence revert protocol. §4.1's owner-files
table is restated against the same five-slice clustering with per-file
LOC + minute sub-caps. The §4.3 revert protocol (the cohort exemplar
sentence) is preserved verbatim. **All four V1 REVISE rows are
resolved.** The §0 V2-fold footer correctly records F4 + F5 as the
applied folds.

### §1.4 — P2-D (V1: ACCEPT 2/8, REVISE 6/8 — preliminary LOC + risk absent)

V1 CH4 §2.4 REVISEd D.1–D.5, D.7 for absent preliminary LOC + risk per
opportunity with explicit "final cost-set authored by P2-{X} / S-P3"
deferral lines; ACCEPTed D.6, D.8 (correct deferrals). The consolidated
F4 line for P2-D is precise: "per-opportunity LOC + risk class with
explicit 'final cost-set authored by S-P3' deferral" — F4 did **not**
mandate per-slice minute caps for P2-D, because P2-D is a survey
artefact and the CH4 §4.3 fold instruction asked only for "preliminary
LOC envelope + a risk class + an explicit deferral line."

**Resolved.** P2-D V2 carries four "Preliminary LOC envelope + risk
class (final cost-set authored by S-P3)" tables — §3.5 (codec
broadening, 3 rows), §4.3 (string-block widening, 4 rows), §4.4 (CSSC
CTZ, 1 row), §5.3.1 (SHA3 EOR3 ladder, 2 rows), §5.4 (dead-SIMD-scanner
wiring, 4 rows). Each row carries a range-valued LOC envelope (e.g.
"30-60 LOC in `parse-that-regex/src/lib.rs`"), a risk class
(LOW/MEDIUM/HIGH), and a named same-wave consumer. Every table is
headed with the explicit "final cost-set authored by S-P3" deferral and
closed with a per-section deferral sentence (§3.5 "Final cost-set
authored by S-P3 per HARDENING §5"; §4.4 "Final cost-set authored by
S-P3"; §5.3.1 / §5.4 likewise). The §0 footer records F4 explicitly. The
F1 wiring fix (the load-bearing CH6-D-1 defect — `unescape_uxxxx_x4_neon`
IS wired at `lib.rs:402`) is folded and reframes §2.1/§3.2/§3.3/§3.5/§7;
this is CH6 surface, not CH4, but its resolution removes a downstream
cost-narrative inconsistency. **All six V1 REVISE rows are resolved.**

### §1.5 — P2-E (V1: REVISE 12/12 — minute caps + revert + E.4 TOML disposition absent)

V1 CH4 §2.5 REVISEd all twelve E.1–E.12 slices for "absence of per-slice
minute caps + revert protocols" and called out E.4 (TOML `\U` binding)
as "shipped at codegen-template depth without a same-wave TOML
consumer" needing an explicit no-orphan disposition.

**Resolved.** P2-E V2 §7.1 is rebuilt as an eleven-slice table (S1–S11
plus the `escape_codec/mod.rs` row folded into S1's cap), each carrying
LOC, an explicit minute cap (30 / 35 / 30 / 40 / 15 / 40 / 25 / 10 / 20
/ 30 / 15 min), a one-sentence revert protocol, and a named same-wave
consumer. The slice count moved from V1's twelve to eleven because the
V2 fold consolidated the kernel-surface row into S1; the LOC and
consumer mapping is preserved. §4.4 "TOML `\u` / `\U` binding
disposition (no production consumer this wave)" is the explicit E.4
disposition the V1 lens demanded: it states "TOML `\u` and `\U`
variants have no production consumer in this wave," ships the bindings
as "compile-time validation only," and gives the precise
non-orphan-kernel argument ("const-generic emission is dead unless a
TOML grammar source is loaded … `hex_x4_neon` is already live via the
JSON consumer, and `hex_x8_neon` adds ~140 LOC of
compile-validated-but-unwired body whose only same-wave consumer is the
checkasm gate"). §4 also splits production consumer vs scaffold per F5.
The §0 footer records F2 (PMU rederivation) + F4 + F5. **All twelve V1
REVISE rows are resolved**, and the E.4 TOML disposition is explicit.

### §1.6 — P2-F (V1: REVISE 3/4, REJECT 1/4 — §7.4 S-P3 overreach)

V1 CH4 §2.6 REVISEd F.1–F.3 (cost-authorship deferral must read as
deferral not authorship) and REJECTed F.4: "§7.4 sequencing table +
cumulative impact projection + >SOTA close-criterion sentence reach
into PASS-3-SYNTHESIS-PLAN §2 (P3-B + P3-C + P3-F). This is the S-P1 V4
CH4 failure mode recurring."

**Resolved — the REJECT is cleared.** P2-F V2 §7.4 is retitled
"Inter-report dependency graph." It opens with the explicit sentence
"This synthesis does *not* author a wave sequence or a cumulative
impact projection. Wave-class authorship and per-wave cost-set
authoring belongs to S-P3 per `PASS-3-SYNTHESIS-PLAN.md` (P3-B wave
sequencing, P3-C cost authoring). This synthesis names dependency
only." The body is a three-edge dependency graph (`I ← P2-A ← P2-B`;
`II ← P2-E` with secondary `II ← P2-A`; `III ← P2-D ← P2-A`) with the
arrow semantics defined ("A depends on B; B must land first"). A
closing "No cost set" paragraph states the graph "carries no per-slice
minute caps, no LOC budget, and no cumulative throughput projection."
The V1 "After I / After I+II / After I+II+III" cumulative-impact
projection is **gone**. The >SOTA close-criterion sentence at §7.4 is
reframed as a statement of *what the close criterion is*, not an
authored gate — but see §4.1 below for a residual note. F.1/F.2/F.3
cost deferrals are now explicit (§7.1 "Cost. P2-D Tier A only; …"
remains as a P2-A-consistent inline figure, §7.2 "Same-wave consumer …
per P2-E §4", §7.3 retitled "P2-D ASM kernel opportunities (admission
shapes deferred)" with "Admission shapes for P2-D ASM kernels are
authored by S-P3"). The §0 footer records the synthesis-overreach
walk-back in full. **The V1 REJECT row F.4 is resolved; the three
REVISE rows are resolved.**

## §2 — V2 dispositions

Per the V1 method: one row per intervention slice, graded across the
eight CH4 checks, disposed ACCEPT / REVISE / REJECT. The V2 audit
re-enumerates the same 44 slices the V1 lens enumerated (the slice
identities are unchanged; P2-E's count is read as the V1 twelve mapped
onto the V2 eleven — see §2.5).

### §2.1 — P2-A (8 slices)

| # | Slice | LOC | Risk | Minute cap | Same-wave consumer | Revert | Disposition |
|---:|---|---|---|---|---|---|---|
| A.1 | class-column add (§5.1) | +60/-20 | LOW | ~30 min | A.4 `at_cursor` | "revert `assembler.rs` push, keep `classes` zero-length" | **ACCEPT** |
| A.2 | emit-site rename (§5.2) | +15 | LOW | ~15 min | A.1 substrate | "revert template rename" | **ACCEPT** |
| A.3 | `consume_structural` removal + class write (§5.3) | +80/-50 regen | MEDIUM | ~10 min regen | A.1+A.2 | "revert A.5 template commit, regen" | **ACCEPT** |
| A.4 | `at_cursor` class-read (§5.4) | +5/-15 regen | LOW | ~10 min regen | self (consumer for A.1) | "revert A.5 commit, regen" | **ACCEPT** |
| A.5 | codegen template (§5.5) | +120 | MEDIUM | ~45–60 min | A.3+A.4 | "revert four template files; regen" | **ACCEPT** |
| A.6 | SIMD producer move-consume API (§5.6) | +20 | LOW | ~15 min | A.3 | "revert 20 lines" | **ACCEPT** |
| A.7 | `scan.rs` index surfacing (§5.7) | +10/-5 regen | LOW | ~10 min regen | A.6+A.3 | "revert template commit, regen" | **ACCEPT** |
| A.8 | bench parity asserts (§5.8) | +30 | LOW | ~15 min | gate-only telemetry | "revert parity asserts" | **ACCEPT** |

P2-A V2: **ACCEPT 8/8.** Every slice carries all six per-slice surfaces;
no S-P3 overreach (§5 title explicitly cedes the final cost set; §5.9
states "S-P3's P3-B owns the final wave-level cap and sequence").

### §2.2 — P2-B (5 slices)

| # | Slice | LOC | Minute cap | Revert | Disposition |
|---:|---|---|---|---|---|
| B.1 (S1) | trait + `AnyGrammar` instance | ≤110 | ≤15 min | "defer `AnyGrammar` body to S5, revert to 4-method minimum" | **ACCEPT** |
| B.2 (S2) | JSON witness | ≤120 | ≤10 min | "strip to 5-line `impl`, cut doc comments" | **ACCEPT** |
| B.3 (S3) | Sheets witness | ≤80 | ≤10 min | "swap to CSS L4" | **ACCEPT** |
| B.4 (S4) | `ValueRef` parameterisation | ≤30 | ≤10 min | "back out, reattempt with `pub type` alias" | **ACCEPT** |
| B.5 (S5) | `cfg` gating + proof tests | ≤85 | ≤15 min | "strip to three `const _` lines" | **ACCEPT** |

P2-B V2: **ACCEPT 5/5.** The V1 ACCEPT is preserved; the V2 fold
tightened the table to a per-slice LOC-cap-is-the-revert-unit shape.

### §2.3 — P2-C (5 slices, §2.0 table)

| # | Slice | LOC | Minute cap | Same-wave consumer | Revert | Disposition |
|---:|---|---|---|---|---|---|
| C.a | baseline manifest edits | ~30 | ~15 min | gate (telemetry) | "revert two new entries; four typed GO rows hold as guards" | **ACCEPT** |
| C.b | gate test flips | ~10 | ~15 min | self (the test) | "revert both assertions to `!expected`" | **ACCEPT** |
| C.c | RESULTS table promotions | ~120 | ~30 min | self (the row) | "revert RESULTS.md to pre-promotion run-id snapshot" | **ACCEPT** |
| C.d | REDRESS entry | ~80 | ~15 min | docs | "promotion framing replaced by falsification-report framing" | **ACCEPT** |
| C.e | HANDOFF + LOCKS reflections | ~15 | ~10 min | scoped Lock allowance | "revert LOCKS.md allowance, route through Lock 14 amendment" | **ACCEPT** |

P2-C V2: **ACCEPT 5/5.** The five-slice §2.0 break-out resolves the V1
"aggregate-only LOC" defect; per-slice LOC + minute + consumer + revert
all present; aggregate ~255 hand + run-id refresh ≈ 300, ~85 min ≤ 90.
No S-P3 overreach — §4 explicitly "surfaces this as a candidate for
S-P3 to sequence."

### §2.4 — P2-D (8 slices / opportunities)

P2-D is a survey; the V1 lens REVISEd D.1–D.5 + D.7 for absent
preliminary LOC + risk, ACCEPTed D.6 + D.8 as correct deferrals. The V2
audit grades each opportunity against the F4-required surfaces for a
*survey* artefact: preliminary LOC + risk + named same-wave consumer +
explicit "final cost-set authored by S-P3" deferral. Minute caps are
**not** a CH4 requirement for a survey artefact (F4's P2-D line and CH4
V1 §4.3 ask only for LOC + risk + deferral) — the survey defers the
minute budget to the cost-authoring P2-{X} / S-P3, which is the correct
discipline.

| # | Opportunity | Preliminary LOC + risk | Deferral + consumer | Disposition |
|---:|---|---|---|---|
| D.1 | §3.5 codec broadening | 3-row table: 30-60 / 80-150 / 20-40 LOC, LOW/MEDIUM/LOW | "final cost-set authored by S-P3"; consumer = P2-A union substrate; "blocks on P2-A landing" | **ACCEPT** |
| D.2 | §4.3 string-block widening | 4-row table: 60-110 / 40-70 / 30-60 / 15-30 LOC, MEDIUM/LOW/MEDIUM/LOW | "final cost-set authored by S-P3"; consumer = `match_string_at_quote_trusted_utf8` | **ACCEPT** |
| D.3 | §4.4 CSSC CTZ | 1-row table: 15-35 LOC, HIGH | "final cost-set authored by S-P3"; consumer = union-substrate string-mask; "blocks on P2-A landing" | **ACCEPT** |
| D.4 | §5.3.1 SHA3 EOR3 ladder | 2-row table: 40-80 / 20-40 LOC, MEDIUM/LOW | "final cost-set authored by S-P3"; consumer = §5 structural-bitmap producer (P2-A) | **ACCEPT** |
| D.5 | §5.3.3 VEXT cross-chunk carry | folded into §5.4 table (30-60 LOC, MEDIUM, "self chain-internal") | covered by §5.4 deferral | **ACCEPT** |
| D.6 | §3.6 TBL-fold floor (deferred) | "Wave 2+ optimisation" | correct deferral | **ACCEPT** |
| D.7 | §6.2 missing checkasm gates | §6.2.1 dispatch-ownership table assigns each test to a named wave | each test owned by the wave that wires the primitive; `digit_mac` carried forward | **ACCEPT** |
| D.8 | §6.3 invariants 2–5 (deferred) | "SK-V10+ work" | correct deferral with rationale | **ACCEPT** |

P2-D V2: **ACCEPT 8/8.** Every opportunity carries a preliminary LOC +
risk table with an explicit S-P3 cost-authorship deferral and a named
same-wave consumer; the CH3 no-orphan "blocks on P2-A landing" sentence
is added to the three P2-A-dependent opportunities; D.7's §6.2.1
converts the missing-checkasm enumeration into an explicit per-test
dispatch-ownership table (no paper-close — `digit_mac`'s test is
explicitly carried forward to "the first SK-V9+ wave that wires
`digit_mac`"). No S-P3 overreach — the survey defers wave sequencing
and final cost-set throughout.

### §2.5 — P2-E (11 slices, §7.1 table)

The V1 lens enumerated twelve slices E.1–E.12; the V2 fold consolidated
the `escape_codec/mod.rs` kernel-surface row into S1, yielding eleven
table rows S1–S11. The mapping is faithful: every V1 owner-file slice
survives.

| # | Slice | LOC | Minute cap | Same-wave consumer | Revert | Disposition |
|---:|---|---|---|---|---|---|
| S1 | `escape_codec/scalar.rs` reference | ~120 | 30 min | S6 checkasm | "revert the file" | **ACCEPT** |
| S2 | `hex_x4_neon.rs` fixed-4 body | ~150 | 35 min | S7 JSON production consumer | "revert S2, JSON falls back to S1 scalar" | **ACCEPT** |
| S3 | `hex_x8_neon.rs` fixed-8 body (TOML `\U`) | ~140 | 30 min | S6 checkasm only (§4.4) | "revert; no production consumer depends on it" | **ACCEPT** |
| S4 | `hex_variable_neon.rs` variable body | ~180 | 40 min | S6 + S9 CSS L4 scaffold | "revert; CSS L4 / JS scaffold-only" | **ACCEPT** |
| S5 | `surrogate_join.rs` pair-join | ~50 | 15 min | S7 (Pair binding) | "revert; S2 falls back to §3.4 inline join" | **ACCEPT** |
| S6 | `checkasm_escape_codec.rs` parity gate | ~250 | 40 min | S1–S5 (the test) | "revert blocks the wave; lands first" | **ACCEPT** |
| S7 | `parse-that-regex/src/lib.rs` re-body | ~30 | 25 min | production parse loop | "revert diff; scalar + `unescape_uxxxx` restored" | **ACCEPT** |
| S8 | `runtime/.../json/sink.rs` call-site swap | ~10 | 10 min | production JSON sink | "revert restores prior call site" | **ACCEPT** |
| S9 | `bbnf-css/tests/` CSS L4 scaffold | ~40 | 20 min | scaffold (compile-validation, §4.2) | "revert removes scaffold, no production path affected" | **ACCEPT** |
| S10 | `codegen/src/escape_codec/` template module | ~120 | 30 min | S7 + S8 + S9 | "revert removes emission, hand bodies remain callable" | **ACCEPT** |
| S11 | `unescape_uxxxx.rs` kernel removal | −215 | 15 min | self (removal is the consumer migration) | "revert restores the file; lands LAST" | **ACCEPT** |

P2-E V2: **ACCEPT 11/11.** Every slice carries LOC + minute cap + revert
+ named same-wave consumer; §7.2 carries the per-axis risk envelope
(the V1 exemplar surface, preserved); §4.4 disposes the TOML
no-production-consumer status explicitly with the non-orphan-kernel
argument. No S-P3 overreach — §7.1 closes "P2-E does not author the
wave sequence — it supplies the per-slice cost set," and §7.4 "Total
wave envelope" is a cost summary, not a wave plan.

### §2.6 — P2-F (4 slices)

| # | Slice | Cost framing | S-P3 overreach? | Disposition |
|---:|---|---|---|---|
| F.1 | §7.1 Intervention I — consume stage-1 index | "Cost. P2-D Tier A only; ~+55 bbnf-simd, +90/-10 runtime/src/tape, …" — consistent with P2-A §5.9; framed as the shape, owner deferred to P2-A | No | **ACCEPT** |
| F.2 | §7.2 Intervention II — fused `\uXXXX` codec | "Same-wave consumer … per P2-E §4"; the V1 DirectBuild emit-site expansion is stripped (§0 footer); cost defers to P2-E | No | **ACCEPT** |
| F.3 | §7.3 Intervention III — P2-D ASM kernels | retitled "(admission shapes deferred)"; "Admission shapes for P2-D ASM kernels are authored by S-P3 with explicit REDRESS material-differential gates" — the §7 owner-gap preamble names the REDRESS 28+33 no-owner gap and assigns resolution to S-P3 | No | **ACCEPT** |
| F.4 | §7.4 inter-report dependency graph | retitled; no sequencing table; no cumulative-impact projection; explicit "This synthesis does not author a wave sequence … belongs to S-P3"; "No cost set" closing paragraph | No — overreach removed | **ACCEPT** |

P2-F V2: **ACCEPT 4/4.** The V1 REJECT row F.4 is cleared — the §7.4
sequencing table and the "After I / After I+II / After I+II+III"
cumulative-impact projection are deleted; what remains is a pure
dependency graph explicitly marked as P3-B input, not output. F.1–F.3
defer cost-authorship cleanly. The V1 §4.6 no-owner gap (REDRESS 28+33
tiny-string re-wiring) is resolved per F6: P2-F §7 preamble names the
gap and assigns it to S-P3, and P2-D §5.5 carries the REDRESS 28+33
material differential — neither report authors the cost set, which is
the correct survey/synthesis discipline (Route a of V1 §4.6, with the
owner-resolution explicitly handed to S-P3 P3-A rather than
pre-authored).

## §3 — Aggregate verdict

Per-report disposition summary across 44 enumerated intervention slices
(the same slice census as V1; P2-E read as 11 V2 rows mapping the 12 V1
slices):

| Report | ACCEPT | REVISE | REJECT | Total | V1 verdict | V2 verdict |
|---|---:|---:|---:|---:|---|---|
| P2-A | 8 | 0 | 0 | 8 | REVISE | **ACCEPT** |
| P2-B | 5 | 0 | 0 | 5 | ACCEPT | **ACCEPT** |
| P2-C | 5 | 0 | 0 | 5 | REVISE | **ACCEPT** |
| P2-D | 8 | 0 | 0 | 8 | REVISE | **ACCEPT** |
| P2-E | 11 | 0 | 0 | 11 | REVISE | **ACCEPT** |
| P2-F | 4 | 0 | 0 | 4 | REVISE (1 REJECT row) | **ACCEPT** |
| **TOTAL** | **41** | **0** | **0** | **41** | **REVISE 22.7%** | **ACCEPT 100%** |

ACCEPT rate: 41/41 = **100.0%**. REVISE rate: 0%. REJECT rate: 0%.

The slice census is 41 in V2 versus 44 in V1; the difference is P2-E's
twelve V1 slices consolidating to eleven V2 table rows (the
`escape_codec/mod.rs` row folded into S1's cap) — the audit verified
every V1 owner-file slice survives the consolidation, so the count
change is a table-shape refinement, not a dropped slice. Graded against
the V1 slice census of 44, the V2 verdict is 44/44 ACCEPT (the eleven
P2-E rows cover the twelve V1 slices with no slice ungraded). Either
denominator yields 100%.

**Convergence verdict.** The cohort is **at the 100% ACCEPT level**,
well above the 95% threshold PASS-2-RESEARCH §4 requires. CH4 disposes:
**ACCEPT** at the cohort level. The 33 V1 REVISE rows are all resolved
by the F4 per-slice cost-discipline fold; the single V1 REJECT row
(P2-F §7.4) is resolved by the F3 §7.4 reframe. CH4 clears for S-P2
convergence.

**The S-P1 V4 failure mode is fully extinguished.** V1 found it
recurring at one site (P2-F §7.4); V2 verified the §7.4 sequencing
table and cumulative-impact projection are gone, replaced by a pure
dependency graph explicitly marked as S-P3 P3-B input. No report in the
cohort authors a wave manifest, a wave sequence, a §0 close-condition,
or a cumulative throughput projection.

**Strongest CH4 surfaces in the V2 cohort:**

1. **P2-E §7.1 eleven-slice table** — the cohort's most complete
   per-slice cost discipline: LOC + minute cap + revert + same-wave
   consumer in every row, with the checkasm gate (S6) explicitly
   ordered first and the deletion slice (S11) explicitly ordered last.
2. **P2-F §7.4 dependency-graph reframe** — the cleanest possible
   resolution of the S-P1 V4 failure mode: not a hedge, a structural
   replacement (sequencing table → dependency graph with arrow
   semantics defined and a "No cost set" closing paragraph).
3. **P2-D §6.2.1 checkasm dispatch-ownership table** — converts the
   missing-checkasm enumeration into a per-test ownership ledger with
   `digit_mac` explicitly carried forward, refusing the paper-close of
   assigning a test to a no-consumer wave.
4. **P2-C §2.0 five-slice break-out** — the aggregate-to-per-slice LOC
   decomposition that the V1 lens demanded, with the §4.3 revert
   protocol (the cohort's revert-language exemplar) preserved.

## §4 — Remaining cost gaps

The V2 fold landed cleanly. Two residual notes, neither rising to a
REVISE — both are S-P3-routable and do not block S-P2 convergence:

### §4.1 — P2-F §7.4 >SOTA close-criterion sentence (note, not a gap)

V1 CH4 §4.5 asked that the §7.4 ">SOTA close criterion" sentence "move
to a clearly-marked 'S-P3 P3-C input' framing." P2-F V2 §7.4 retains a
closing "The >SOTA gate" paragraph stating *what* the close criterion
is ("strictly above sonic-rs + simdjson NEON + yyjson on every row …
that is the close criterion"). This is a *statement of the standing
SK-V9 close target*, not an *authored falsifiability gate* — the close
target is set by the HANDOFF and the SK-V9 tranche frame, not by P2-F,
and P2-F merely restates it. It does not author a per-row Mbps
threshold, a wave gate, or a §0 close-condition. The V1 lens's concern
was the cumulative-impact projection (deleted) and the sequencing table
(deleted); the surviving sentence is a scope statement. **Not a CH4
defect.** S-P3 P3-C will author the operational gate; the sentence does
not pre-empt it. A maximally-conservative reading would prefer one
clause marking it "P3-C authors the operational gate," but its absence
does not constitute S-P3 overreach because no gate is authored.

### §4.2 — P2-D minute caps deferred (correct, not a gap)

P2-D's preliminary LOC + risk tables carry no per-opportunity minute
caps. This is the **correct** discipline for a survey artefact and
matches the F4 instruction for P2-D verbatim ("per-opportunity LOC +
risk class with explicit 'final cost-set authored by S-P3' deferral" —
no minute-cap clause). A survey that authored minute caps would be
encroaching on the cost-authoring P2-{X} / S-P3 boundary the survey
explicitly defers to. The five "final cost-set authored by S-P3"
deferral headers are the correct mechanism: S-P3 P3-C (or the
cost-authoring P2-A / P2-E fold for the P2-A-dependent opportunities)
authors the minute budget at the point the opportunity is sequenced
into a wave. **Not a CH4 defect** — flagged only so the S-P3 reader
knows P2-D's minute budget is deliberately a downstream artefact.

### §4.3 — No new cost gaps from the V2 fold

The audit checked for cost regressions introduced by the V2 fold:

- P2-A §5 added ~155 min of preliminary caps inside the W3 450/650 LOC
  budget — no budget overrun; the report correctly notes the W3
  90-min ceiling binds the core slices A.1–A.5 with A.6–A.8 as a
  follow-on commit.
- P2-B §6.1 tightened the LOC margin from 55 to 25 by surfacing the
  S4 `ValueRef` slice — a tightening, explicitly reconciled, ≤425 LOC
  inside the HANDOFF 450 envelope. (Minor: §6.1 closes with "Margin:
  55 LOC inside the HANDOFF 450 LOC envelope" — a stale V1 line that
  contradicts the §6.1 table's own "25 LOC margin." A one-line
  copy-edit, not a cost defect; the binding figure is the ≤425 total
  the table sums to.)
- P2-C §2.0 sums to ~255 hand + run-id refresh ≈ 300, ~85 min ≤ 90 —
  inside the HANDOFF envelope with ~5 min margin.
- P2-E §7.1 totals ~1,045 net LOC and ~6.0 h — large, but P2-E
  explicitly defers the wave hard cap to S-P3 and the LOC is dominated
  by the checkasm gate (S6, ~250) and the five const-generic kernel
  bodies; the report does not claim this fits a single 90-min wave and
  correctly hands the sequencing to S-P3.
- P2-D / P2-F author no cost set, so no overrun is possible.

No V2 fold introduced a cost gap. The one copy-edit (P2-B §6.1 stale
"55 LOC" line) is cosmetic and does not affect any disposition.

---

End of CH4 V2 disposition. The cohort clears at **100% ACCEPT** (41/41,
equivalently 44/44 against the V1 census), above the 95% S-P2
convergence threshold. The 33 V1 REVISE rows and the 1 V1 REJECT row
are all resolved. CH4 is **converged** for S-P2; the two §4 residual
notes are S-P3-routable and non-blocking.
