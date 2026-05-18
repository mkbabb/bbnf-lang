# SK-V9 S-P1 V4 Hardening — CH3 REGRESSION Disposition

Lens: CH3 REGRESSION (per `restart/prompts/ORCHESTRATOR.md` §3W).
Scope: V4-folded SK-V9 S-P1 cohort — the six reports
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md` as folded
per `HARDENING-S-P1-V3-CONSOLIDATED.md` F1–F6.
V3 CH3 baseline: CONDITIONAL ACCEPT at 91.7% strict / 97.2% lenient
(33 A / 2 WATCH / 2 REVISE / 0 REJ across 36 dispositions, with the two
REVISEs concentrated in P1-V3-D §6.1 W1 and §6.3 V10).
Authority cross-checked: `skinny/REDRESS.md` entries 1–93,
`restart/skinny/tranches/sk-v9/HANDOFF.md` §5,
`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md`.
Verdict vocabulary: ACCEPT / WATCH / REVISE / REJECT — bound to a REDRESS
citation and a material-differential argument.
Disposition: read-only. No edits, no commits.

---

## §1 — V3-disposition-resolution

The V3 CH3 disposition closed 33 ACCEPT, 2 WATCH (C2 noinline build, D2
W2 framing), 2 REVISE (D1 W1 string-plane, D3 V10 unicode), 0 REJECT.
This section verifies, point-by-point, the eight V4 fold expectations
named in the dispatch.

### §1.1 — V3 CH3 REVISEs resolution (dispatch Q1)

The V3 REVISEs were P1-V3-D §6.1 (W1 string-plane masked-bitmap cost cut)
and P1-V3-D §6.2 (V10 unicode validation kernel). Both soft-reopened
REDRESS-rejected classes without explicit cite + material differential.

**Verification of V4 fold (F3 + F1) for §6.1 → §6.1 in V4 D:**

The V4 P1-V3-D adds **REDRESS material differential note (F3, CH3 D-1;
CH5 §4.1, F6)** at its §6.1 (now titled "Parse_only LOSS-block finding:
per-string-span-delimiter cost dominates"), reading (verbatim):

> A candidate intervention on this finding REPLACES the existing string-
> scanner pair on the production hot path — `match_tiny_plain_string_with_cap`
> at `runtime/src/grammars/json/generated.rs:171-185` and
> `match_string_at_quote_trusted_utf8` at `parse-that-regex/src/lib.rs` —
> running alongside the existing scanner constitutes a sidecar producer
> and fails Lock 1 (substrate cardinality stays at one; per `LOCKS.md` Lock
> 1 a "SIMD mask stream is a transient producer, not a retained sidecar").
> The string-scanner-widening class on these same rows was rejected by
> REDRESS 60 (boundary collapse), 61 (always-wide retained trusted scan),
> 62 (delayed-wide retained trusted scan), 83 (StringBlock16 tiny probe),
> and 84 (object-pair value-byte control compaction). The retained
> Unicode-escape run validator was rejected by REDRESS 64. Any successor
> intervention must demonstrate a material differential against each
> cited rejection on a same-row falsification gate; this report stops at
> the diagnostic, and wave-class authoring belongs to S-P3 per F1.

Cross-check vs REDRESS, each citation:

- **REDRESS 60** (boundary collapse): rejects retained trusted-string
  boundary collapse on `unicode_*` / dense-key rows. The V4 D §6.1
  finding names the same row class (the 11 parse_only LOSS rows). The
  shape covered (a single masked-bitmap pass across `b'"'`, `b'\\'`,
  `<0x20`) is exactly the boundary-collapse signature 60 rejected.
  Cite + differential present.
- **REDRESS 61** (always-wide retained trusted scan): rejects the
  64-byte AArch64 quote/backslash/control scanner inside
  `parse-that-regex::skip_json_string_plain_trusted` measured at
  `+16.9%` / `+15.8%` / `+6.0%` Track 1 on the same `unicode_mixed` /
  `gsoc-2018` / `y_string_unicode` triple the §6.1 finding targets. The
  V4 D §6.1 names `match_string_at_quote_trusted_utf8` and explicitly
  says the candidate must REPLACE not run alongside — a direct
  invocation of 61's failure mode. Cite + differential present and
  material.
- **REDRESS 62** (delayed-wide retained trusted scan): rejects the
  delayed-entry shape with the first 16-byte AArch64 trusted-string
  probe preserved. Same row triple, same failure mode. Cite present;
  differential is by construction (the §6.1 wording "REPLACES the
  existing string-scanner pair" excludes the delayed-entry shape since
  it would preserve the existing tiny scanner).
- **REDRESS 64** (retained Unicode-escape run validator): rejects the
  per-run scalar validator. V4 D §6.1 names "escape-complete scan
  (per-byte branch over `b'\\'` and `<0x20`)" as one of the hot
  primitives; the §6.1 note routes any successor to a falsification
  gate. Cite + differential present.
- **REDRESS 83** (StringBlock16 tiny probe): rejects the 16-byte
  generated-retained wrapper at `match_tiny_plain_string_with_cap<16>`
  call site — measured `-36.0%` / `-34.1%` / `-37.2%` Track 1 on the
  same `twitter` / `update_center` / `unicode_basic` rows. V4 D §6.1
  names the exact file:line (`runtime/src/grammars/json/generated.rs:171-185`).
  Cite + differential present and material.
- **REDRESS 84** (object-pair value-byte control compaction): rejects
  the W6 control-byte compaction route on object pairs. V4 D §6.1 cites
  this as part of the umbrella; the differential is that §6.1 stops at
  diagnostic naming and explicitly forbids reopening the route without
  a same-row falsification gate.

All five REDRESS-rejection citations are bound by SHA-anchored measured
evidence (61 the `unicode_mixed +16.9% / gsoc +15.8% / y_string +6.0%`
parse-attribution gate; 83 the `twitter -36% / update_center -34% /
unicode_basic -37%` Track 1 regression gate). The §6.1 differential is
concrete: the V4 finding **stops at diagnostic naming and defers
intervention to S-P3** per F1, which is precisely the discipline the V3
CH3 REVISE called for.

**Verification of V4 fold (F3 + F1) for §6.2 → §6.2 in V4 D:**

The V4 P1-V3-D adds **REDRESS material differential note (F3, CH3 D-3)**
at its §6.2 (now titled "Unicode-row finding: per-quartet primitive
class dominates residual"), reading:

> REDRESS 82 rejected the four-`\uXXXX` AArch64 classifier on exactly
> these rows. REDRESS 59 permanently rejected the UTF-8 fusion class on
> the close route. Any successor intervention must articulate the
> differential against each cited entry on a same-row falsification
> gate. Wave-class authoring belongs to S-P3 per F1.

Cross-check:

- **REDRESS 82** (single-quartet Unicode escape classifier): rejects
  the W4 single-quartet classifier reusing
  `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` on exactly
  `unicode_escapes` / `unicode_mixed` / `y_string_unicode` (gate failed
  at 82.1% / 49.9% / 64.0% of sonic). V4 D §6.2 names exactly those
  rows and routes any successor to the falsification gate. Cite +
  differential present and material.
- **REDRESS 59** (UTF-8 fusion class refuted): the rejection is
  "permanent" per REDRESS preamble; V4 D §6.2 echoes the permanence
  language. Cite present; differential is by construction (§6.2 stops
  at diagnostic naming).

The §6.2 V4 fold satisfies CH3 D-3 with both halves of §3W gating: cite
explicit, differential material (route-shape vs same-row falsification
gate).

**V3 REVISE resolution verdict: both V3 REVISEs (D1, D3) close.** The
V4 fold satisfies the CH3 requirement that any soft-reopened class
carry an explicit REDRESS cite plus an articulable material differential
(or be demoted to diagnostic-pending-falsification-gate).

### §1.2 — D §6.6 wave-prescription strip (F1) didn't sneak back (Q2)

V3 §6.6 was "three V9/V10 waves, ranked" — the load-bearing CH4 / CH3
root-cause language. V4 D §6.6 reads:

> Wave-class selection and per-wave cost set (LOC, risk, owner files,
> same-wave consumer, revert) are S-P3 scope per
> `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`. This S-P1 report
> supplies the diagnostic findings; S-P3 picks waves.

The wave proposals are stripped verbatim. **Verification that the wave
shape did not migrate into §6.1 or §6.2:**

- §6.1 is titled "Parse_only LOSS-block finding"; finding-class, not
  wave-class. The closing sentence routes wave authoring to S-P3.
- §6.2 is titled "Unicode-row finding: per-quartet primitive class
  dominates residual"; same shape, same S-P3 routing.
- §6.3 is the WIN-row guard (citm/canada/mesh/marine_ik/numbers/
  instruments). No wave language.
- §6.4 is the direct-plane decorrelation finding with REDRESS 66–69 +
  93 umbrella citation; no wave language.
- §6.5 is the typed-plane 4/4 GO admission guard; no wave language.

No wave-prescription language leaks via §6.1, §6.2, or any other §6
subsection. F1 holds cleanly.

### §1.3 — D §6.5 "redesign → profile" language (Q3)

The V3 CH3 D2 WATCH targeted the D2 framing ("a separate wave should
profile the digest producer") to ensure it stayed profile-only. The
V4 fold renumbered: what was V3 §6.5 (digest sink) is now V4 §6.4
("Direct-plane finding: q/B decoupled from digest gap").

V4 §6.4 reads:

> The direct plane is `q/B`-decorrelated (r = −0.033). The direct LOSSes
> ... do not load on the per-string-span-delimiter plane; they live in
> the digest-sink-producer cost profile. Per the §4 F3 note, the
> digest-sink-redesign class is closed by REDRESS 66–69 + 93; any
> further direct-plane work routes to a dedicated direct-output-
> contract or control-path tranche.

And the §4 F3 note (V4 §4.1) reads "The direct-plane decorrelation is a
*diagnostic finding*; it is not a proposal to redesign the digest
path." — exactly the "profile not redesign" framing the V3 CH3 §4.3
fold-ask prescribed. The word "redesign" appears only as a
*forbidden* shape, with REDRESS 66–69 + 93 cited as the binding
constraint and the direct-output-contract tranche routed as the only
admissible follow-up.

**Verification: V4 D's digest-sink language reads "profile not
redesign" with explicit REDRESS-66–69 + 93 binding.** F1 (no wave
authoring) and F3 (cite + differential) both hold for this section.

### §1.4 — E §2 SAFE-TO-DELETE NEON vs scalar distinction (Q4)

V3 CH3 §2.5 E1 verified the V3 E correctly distinguished the rejected
NEON `match_tiny_plain_string` kernel (REDRESS 28+33) from the admitted
scalar `match_tiny_plain_string_with_cap<16>` (REDRESS 72). The V4
fold expectation (F4 = generality, but the V4-D-NEON distinction is
the CH3 angle): V4 E should explicitly distinguish REDRESS 28+33 NEON
kernel deletion from REDRESS 72 scalar admit retention.

V4 E §2.2 reads (verbatim, the `match_tiny_plain_string.rs` row):

> SAFE-TO-DELETE per REDRESS 28+33 (active NEON `match_tiny_plain_string`
> kernel rejected as retained parse-G fix) + REDRESS 72 (admitted scalar
> shape lives in generated runtime, not as a NEON primitive). **Critical
> distinction**: this row deletes the **NEON kernel**
> `match_tiny_plain_string_neon` at
> `bbnf-simd/src/aarch64/match_tiny_plain_string.rs` (REDRESS 28+33
> rejection). It does NOT touch the **admitted scalar**
> `match_tiny_plain_string_with_cap::<16>` at
> `runtime/src/grammars/json/generated.rs:171-185` and
> `codegen/src/json_templates/generated.rs:171-185` (a 4-line scalar
> loop admitted by REDRESS 72). The two surfaces are independent: the
> NEON file is deleted; the scalar inside generated.rs stays.

The distinction is explicit — file path NEON vs file path scalar, "the
NEON file is deleted; the scalar inside generated.rs stays" — and is
reaffirmed in V4 E §4.3 step 5 ("one commit for aarch64 NEON
`match_tiny_plain_string` (NEON kernel only — the scalar in
`runtime/src/grammars/json/generated.rs:171-185` is NOT touched)").

**Verification: V4 E satisfies the NEON-vs-scalar separation, with
file:line evidence on both sides and per-ISA-family commit granularity
guaranteeing the rejection (28+33) and the admit (72) cannot be
silently re-merged.** No REDRESS 72 scalar admit is endangered.

Note also: V4 E §2.3 carries REDRESS 72 forward implicitly by listing
the *scalar* reference modules under §2.5 KEEP, and §2.4 lists every
admitted aarch64 LIVE primitive with its consumer cite. The
SAFE-TO-DELETE blast radius is bounded to orphan kernels.

### §1.5 — F §2 SUPERSEDED reasoning expansion (Q5)

V3 §2.13 listed seven SUPERSEDED entries (35, 36, 37, 38, 46, 49, 70)
with one-line supersession claims. V4 §2.13 expands each to a multi-
line supersession-chain reasoning naming the admit/reject SHA citations
and the shape differential. Cross-check:

- **35 → 40 + 48 + 71 + 81**: V4 explanation distinguishes "diagnosis"
  (35's "scaffolding gap") from "delivered generator" (40+48+71+81's
  admits). Shape differential is named.
- **36 → 85 + 86**: V4 explanation contrasts "JSON-name presence in a
  generic crate" (36's diagnosis) vs "JSON-name removal under the
  Lock 14 fence" (85/86's admit). Shape differential is named.
- **37 → 85 + 86**: Same Lock 14 Phase A-D chain; god-module structure
  vs codegen-shell structure. Named.
- **38 → SK-V5 NUKE-PLAN Wave 4**: filesystem-layer "directory does not
  exist" verification cited (P1-V3-E §2.7). The shape diff is "fossil
  directory exists" vs "directory removed". Named.
- **46 → 71 + 81**: context-sink proposal vs typed-path generator land
  the same outcome at row level (`real_typed_struct A/GO` on numeric
  corpora). Shape differential named.
- **49 → 66**: surface retained, route closed. Shape differential
  ("surface exists but route is closed") explicit; "the admit is no
  longer a forward producer" is a concrete material differential.
- **70 → 71**: "first attempt failed on architectural grounds" vs
  "second attempt landed under the host/API schema lesson". Shape diff
  named; the architectural lesson is preserved in 70 but the live
  producer is 71.

Each supersession-chain explanation now reads as "old shape → new
shape" with the binding constraint relocated rather than diluted.
**Verification: V4 F's expanded SUPERSEDED reasoning satisfies the
two-halves §3W discipline (cite + differential) for all seven
entries.** No silent admit; no rejected route revived.

### §1.6 — No silent regressions / 4 typed-GO + 3 direct-GO protection (Q6)

The admitted-row protection table from V3 CH3 §6 was: 4 typed-GO
(twitter REDRESS 71, update_center REDRESS 71, mesh REDRESS 81,
marine_ik REDRESS 81) + 3 direct-GO (citm_catalog, apache_builds,
github_events, with the count caveat that V3 noted 3 or 4 depending on
instruments band).

V4-D §6.3 reads:

> citm_catalog, canada, mesh, marine_ik, numbers: WIN unconditionally
> on parse_only ... any successor wave must guard them. REDRESS 71
> (twitter, update_center typed-GO) and REDRESS 81 (mesh, marine_ik
> typed-GO) bind the admitted-row guard.

V4-D §6.5 reads:

> All 4 measured typed rows admit (GO). Track 2 oracle parity at 14,977
> (twitter) and 9,796 (marine_ik) confirms structural soundness ... and
> forbids a substrate-change follow-up.

The four typed-GO rows are guarded explicitly. The three direct-GO
rows are guarded in V4-D §6.4 ("any further direct-plane work routes
to a dedicated direct-output-contract or control-path tranche") plus
§4.1's apache_builds +16.6% / unicode_basic +14.5% / marine_ik +9.7% /
citm_catalog +7.9% direct-WIN naming.

**Verification: no V4 finding silently regresses any of the seven
admitted rows.** The §6.1 / §6.2 string-plane and unicode findings
explicitly defer wave authoring to S-P3 (F1) so cannot dispatch a
perturbing wave; the §6.3 / §6.4 / §6.5 guards are explicit.

### §1.7 — No new REDRESS reopens (Q7)

Scanning the V4 fold deltas (V3 → V4) for any new soft-reopen
introduced by the fold itself:

- V4-A: no new REDRESS-route reopens; the V4-A fold added cited
  disposition source + bound the PMU manifest to F6 (Lock-1 substrate
  binding).
- V4-B: V4-B fold added the **primitive-class vocabulary** (Lock 14 /
  Lock 16 reframing of the JSON-named symbols as
  `string_tiny_scan` / `string_full_scan` / `escape_codec_hex_unit` /
  etc.). This is a **generality fold** (CH2 lens), not a route
  proposal. No reopen — substrate-neutral renaming reinforces the F4
  CH2 fold and does not weaken any pre-block.
- V4-C: V4-C fold refolded SC-1 / SC-4 against the V3-A/B PMU rows.
  The §4 (V4) verdict on SC-1 removes the `#[inline(never)]` probe
  proposal entirely ("A's PMU rows × B's TP per-class shares already
  provide the cycle attribution") — see §1.8 below. The §5 (V4)
  verdict on SC-4 reframes the literal 75% claim as "47–67% on the
  dense-key losses" with no new substrate proposal. No reopen.
- V4-D: covered in §1.1–§1.3.
- V4-E: covered in §1.4 + V3 CH3 §2.5 already at ACCEPT; V4 fold
  preserves the SAFE-TO-DELETE list verbatim and adds the
  primitive-class-status column per F4 (CH2). No new reopen.
- V4-F: covered in §1.5; F's V4 fold drops the
  `prompts/skinny/PASS-1-PROFILE.md` edit (orchestrator-scope
  violation), strengthens the umbrella additions, and reconciles the
  edit count to 19. No reopen.

**Verification: no new REDRESS reopens introduced by the V3 → V4
fold.** The fold is monotonic; it narrows the cohort's posture from
"wave proposals" to "diagnostic findings" without admitting any new
route.

### §1.8 — D §6.1 Lock-1 binding sentence (F6) (Q8)

F6 prescribed: "P1-V3-D §6.1 explicit Lock-1 binding: the proposed
string-plane bitmap REPLACES `match_tiny_plain_string_with_cap` /
`match_string_at_quote_trusted_utf8` on the production hot path — not
alongside; substrate cardinality stays at one."

V4 P1-V3-D §6.1 (the F6-binding sentence) reads:

> A candidate intervention on this finding REPLACES the existing string-
> scanner pair on the production hot path — `match_tiny_plain_string_with_cap`
> at `runtime/src/grammars/json/generated.rs:171-185` and
> `match_string_at_quote_trusted_utf8` at `parse-that-regex/src/lib.rs` —
> running alongside the existing scanner constitutes a sidecar producer
> and fails Lock 1 (substrate cardinality stays at one; per `LOCKS.md` Lock
> 1 a "SIMD mask stream is a transient producer, not a retained sidecar").

The sentence:

1. Names both production hot-path consumers by file path + line range.
2. Explicitly says "REPLACES ... not alongside".
3. Cites Lock 1 by name.
4. Names the failure mode of the alongside-shape ("constitutes a
   sidecar producer and fails Lock 1").
5. Quotes the Lock 1 substrate-cardinality clause verbatim.

The C2 V3 WATCH (noinline build measurement-only) is folded out by
V4-C §4.2's removal of the `#[inline(never)]` requirement ("no longer
required — A's PMU rows × B's TP per-class shares already provide the
cycle attribution"). The WATCH is closed at the **source** (the probe
proposal is gone), not the constraint level, which is the strongest
form of closure.

**Verification: F6 substrate-cardinality discipline is satisfied with
explicit Lock 1 quotation and "REPLACES not alongside" framing. C2
WATCH closed by deletion.**

---

## §2 — V4 dispositions

Each row records (report, V4 finding/proposal, REDRESS class the row
would touch if reopened, V4 cite status, differential argument,
verdict). Rows are grouped by source report.

### §2.1 — P1-V3-A (V4 fold) — 5 dispositions

| # | V4 finding / proposal | REDRESS class | V4 cite | Differential | Verdict |
|---|---|---|---|---|---|
| A1 | V4 fold footer (§0) adds cited disposition source + binds PMU manifest to F6 Lock-1 substrate binding. | None — diagnostic non-producer. | Explicit. | A1's fold reinforces the SPEC §1 invariant; PMU c/B rows characterise, not produce. | ACCEPT. |
| A2 | xctrace probe at `bbnf-bench/src/bin/xctrace_probe.rs` with steady-state inner loop. | REDRESS 72 admitted shape characterised at steady state. | Implicit (V3 still). | A2 unchanged from V3 — sanity parse before PMU read; cycles via `ri_cycles` deltas, not ns→c/B inference. | ACCEPT. |
| A3 | Per-symbol PMC attribution unavailable via `xctrace export`; samply V2 per-symbol % is reused as the attribution lane. | Hot-leaf attribution class. | Implicit. | Description-only; no new attribution leaf proposed. | ACCEPT. |
| A4 | `y_string_unicode` 4.4% residual finding REMOVED per F5 (samply artefact; B falsifies at symbol level). | REDRESS 91 telemetry-overclaim. | Explicit by deletion. | The CH1 defect-removal strengthens REDRESS 91 discipline; harness frame disambiguated. | ACCEPT. |
| A5 | `distinct_values` c/B arithmetic corrected `2.88 → 3.85` per F5; TP path citation reconciled; corpus-name shear `update-center` ↔ `update_center` resolved canonically. | None — telemetry hygiene. | Explicit. | CH1 defect fixes that do not perturb any admitted route. | ACCEPT. |

### §2.2 — P1-V3-B (V4 fold) — 5 dispositions

| # | V4 finding / proposal | REDRESS class | V4 cite | Differential | Verdict |
|---|---|---|---|---|---|
| B1 | xctrace Time Profiler attributes 24–62% of self-samples to `match_tiny_plain_string_with_cap<16>` on string-heavy rows. | REDRESS 72 admitted shape. | Explicit. | Observation of admitted shape; no widening proposal. | ACCEPT. |
| B2 | Track 2 hand parser attributes 30–63% to `match_tiny_plain_string` (cap-8). | REDRESS 72 split holds. | Implicit. | Same observation discipline; cap-8/cap-16 split preserved. | ACCEPT. |
| B3 | V4-B added **primitive-class vocabulary** (Lock-14-fold per F4): `string_tiny_scan`, `string_full_scan`, `escape_codec_hex_unit`, `digit_fsm`, `structural_walker`, etc. | None — generality fold (CH2 lens), not a route proposal. | Explicit at §0 + §125 + §1140 footer. | Substrate-neutral renaming. Each JSON-named symbol maps to a substrate-neutral primitive class admitting CSS L4 / Sheets / future grammars. No reopen of any rejected route. | ACCEPT. |
| B4 | "75%" SC-4 claim reframed as "string_tiny_scan + string_full_scan combined share". | SC-4 (P2 substrate ceiling). | Implicit. | Descriptive reframe; no widening or fusion proposal. | ACCEPT. |
| B5 | citm_catalog/track2, mesh/track2 <5% in `string_tiny_scan`. | REDRESS 70/71/81 admitted typed-GO rows. | Implicit. | Corpus-mix observation; no admit/reject claim. | ACCEPT. |

### §2.3 — P1-V3-C (V4 fold) — 5 dispositions

| # | V4 finding / proposal | REDRESS class | V4 cite | Differential | Verdict |
|---|---|---|---|---|---|
| C1 | V4-C refold against V3-A/B PMU rows (F2). SC-1 *non-fusion* claim holds at both symbol and cycle layers (`scan_structurals` c/B = 0.00 on 34/34 rows). | REDRESS preamble Fact 2 + REDRESS 56 scan-floor admit. | Explicit at §4.2. | Substrate-neutral confirmation; structural-scan stays a non-consumed diagnostic. | ACCEPT. |
| C2 | `#[inline(never)]` falsification probe **REMOVED** per V4-C refold ("no longer required — A's PMU rows × B's TP per-class shares already provide the cycle attribution"). | REDRESS 50–55 retained projection class — the C2 V3 WATCH risk is closed. | Explicit at §4.2 closing sentence. | The probe proposal is deleted; no production-path inline barrier in flight. V3 CH3 WATCH closed by deletion. | ACCEPT (closes V3 WATCH). |
| C3 | "Honest range 47–67% on dense-key losses" SC-4 reframe; literal 75% pair share demoted to upper bound. | SC-4 + REDRESS 55 fused materializer. | Explicit at §5 (V4 refold). | Reframe is measurement-rebased; no fused materializer proposal. | ACCEPT. |
| C4 | Spearman ρ = +0.755 string-fraction vs de-fused string share. | REDRESS 60–62, 66–69. | Explicit (V4 refold cites REDRESS umbrellas). | Correlation finding; intervention defer to S-P3. | ACCEPT. |
| C5 | "y_string_unicode 4.4% residual split across mach_absolute_time / _platform_memmove / libsystem_malloc" — V4 fold reframes per F5. | REDRESS 91 telemetry-overclaim. | Explicit. | Harness frame disambiguated; no parser-leaf claim. | ACCEPT. |

### §2.4 — P1-V3-D (V4 fold) — 8 dispositions

| # | V4 finding / proposal | REDRESS class | V4 cite | Differential | Verdict |
|---|---|---|---|---|---|
| D1 | V4 §6.1 "per-string-span-delimiter cost dominates" diagnostic finding. F1 demoted wave-prescription to S-P3. F3 added REDRESS 60/61/62/64/83/84 cite block. F6 added Lock-1 "REPLACES not alongside" binding sentence. | REDRESS 60/61/62/64/83/84. | Explicit at §6.1 REDRESS material differential note. | "REPLACES the existing string-scanner pair on the production hot path ... running alongside ... fails Lock 1" + file:line citations + same-row falsification-gate routing. Material differential: V4 demotes intervention to S-P3 scope; the §6.1 prose stops at finding. | ACCEPT (closes V3 REVISE). |
| D2 | V4 §6.2 "per-quartet primitive class dominates unicode residual" diagnostic finding. F3 added REDRESS 59 + 82 cite block. F1 demoted wave to S-P3. | REDRESS 59 + 82. | Explicit at §6.2 REDRESS material differential note. | Same-row falsification gate required for any successor; wave-class authoring deferred. | ACCEPT (closes V3 REVISE). |
| D3 | V4 §6.4 "direct-plane finding: q/B decoupled from digest gap" — F3 reframed digest path as "profile not redesign" with REDRESS 66–69 + 93 binding. | REDRESS 66–69 + 93. | Explicit at §6.4 + §4.1 F3 note. | "It is not a proposal to redesign the digest path"; direct-output-contract tranche routing. | ACCEPT (closes V3 WATCH). |
| D4 | V4 §6.3 WIN-row guard (citm/canada/mesh/marine_ik/numbers/instruments). | REDRESS 71 + 81 + preamble Fact 2 + REDRESS 56. | Explicit at §6.3. | Admitted-row guard preserved; no perturbation proposed. | ACCEPT. |
| D5 | OLS coefficients `~1.08 ns/delimiter`, `~0.18 ns/token`, baseline `~0.051 ns/B`. F5 committed regression script at `/tmp/skv9-xctrace-v3/regression.py` + output at `regression_output.json`. | REDRESS 81 number-FSM guard. | Implicit + §5.5 explicit. | Number-FSM stays strongest sub-plane; REDRESS 80 / 81 preserved. | ACCEPT. |
| D6 | V4 §6.5 typed-plane 4/4 GO admission guard. | REDRESS 71 + 81 + 91. | Explicit at §6.5. | "Horizontal follow-up only; forbids substrate-change follow-up." | ACCEPT. |
| D7 | V4 §6.6 wave authoring deferred to S-P3 (F1). | None — meta-discipline. | Explicit. | No wave-language in §6.1–§6.5; S-P3 owns wave selection. | ACCEPT. |
| D8 | F5 OLS R²=0.371 with p_b=0.54 caveat on numeric-token coefficient. | None — diagnostic caveat. | Explicit at §5.1. | OLS caveat is named; coefficients are not admitted as wave knobs. | ACCEPT. |

### §2.5 — P1-V3-E (V4 fold) — 6 dispositions

| # | V4 finding / proposal | REDRESS class | V4 cite | Differential | Verdict |
|---|---|---|---|---|---|
| E1 | V4 split into E1 (doc, LOW risk, ≤30 min, no `cargo test` gate) + E2 (code, MEDIUM risk, ≤45 min + mandatory `cargo test --workspace --profile ax-iter` + xtask checks + per-ISA-family commit + `git revert` on failure). | None — dispatch discipline (CH4 fold). | Explicit at §0 + §1 + §2. | E2 depends on E1 closure for path stability; deletion blast radius bounded. | ACCEPT. |
| E2 | SAFE-TO-DELETE `aarch64/match_tiny_plain_string.rs` (NEON kernel only). | REDRESS 28+33 NEON reject + REDRESS 72 scalar admit. | Explicit "Critical distinction" block. | "The NEON file is deleted; the scalar inside generated.rs stays." File:line both sides. | ACCEPT. |
| E3 | SAFE-TO-DELETE 14 x86_64 `unimplemented!()` shells; 2 (avx512_vpclmul, avx_ifma) carry REJECTED-CLASS status per REDRESS 88 / 80; 12 carry "N/A — placeholder, never admitted". | REDRESS 50–55 admission rule + 80 + 88. | Explicit per-row. | Class-status column added per F4 (CH2 fold); each row distinguishes corpus-scoped vs REJECTED-CLASS. | ACCEPT. |
| E4 | KEEP-IF-USED `string_block.rs` (parse-that-regex consumer at `lib.rs:472, 551`). | REDRESS 61/62/83 reject + REDRESS 42 admit (trusted UTF-8 boundary). | Explicit at §2.2 R1 + §6 R1. | Two surfaces (rejected retained-G wrapper vs admitted parse-that-regex utf8 path). | ACCEPT. |
| E5 | KEEP `unescape_uxxxx::unescape_uxxxx_x4_neon` (parse-that-regex consumer at `lib.rs:402, 419`). | REDRESS 64+82 reject vs admitted materialization path. | Explicit at §6 R2 + class-status as `escape_codec_hex_unit` primitive class. | Materializer surface ≠ rejected validator wrapper. | ACCEPT. |
| E6 | `simd-scan/` claim corrected per F5 ("V3 prose incorrectly framed this as 'empty directory'; the correct framing is the SK-V5 NUKE-PLAN already retired the crate"). | REDRESS 38 SUPERSEDED. | Explicit at §2.7. | F5 CH6 defect fix; no reopen. | ACCEPT. |

### §2.6 — P1-V3-F (V4 fold) — 7 dispositions

| # | V4 finding / proposal | REDRESS class | V4 cite | Differential | Verdict |
|---|---|---|---|---|---|
| F1 | V4 §2.13 SUPERSEDED reasoning **expanded** per F5: each of entries 35, 36, 37, 38, 46, 49, 70 now carries multi-line supersession-chain reasoning with admit SHA citations and shape differential. | All seven SUPERSEDED entries. | Explicit per entry. | Each "old-shape → new-shape" pair carries binding-constraint relocation reasoning. | ACCEPT. |
| F2 | V4 §3.2 four HANDOFF §5 class-umbrella additions: string-scanner-widening, direct-receiver/scratch/semantic-fact, bench-private-hand-Track-1, PMU-as-producer. | REDRESS 60-65/82-84, 66-69, 34+70, SPEC §1 non-negotiables. | Explicit. | Each umbrella cites the binding REDRESS entries; no admit-route created. | ACCEPT. |
| F3 | V4 strictness-plane assertion made explicit at §2 — every comparator delta carried into reconciliation sources `strictness=strict, freshness=same-run-native`. | None — telemetry hygiene. | Explicit. | F5 CH1 fold; sidecar/permissive/`utf8_lossy` rows explicitly NOT behavior-admission ancestors. | ACCEPT. |
| F4 | V4 edit-count rollup reconciled to 19 (8 SPEC + 6 HANDOFF + 5 DISPATCH-PROMPT); SPEC §4.1 Edit E demoted to deferral decision (not a surgical edit) per F5. | None — F-internal arithmetic. | Explicit at §4.4 + §7. | F5 CH1 fold. | ACCEPT. |
| F5 | V4 PASS-1-PROFILE.md edit dropped per F5 (orchestrator-scope violation per ORCHESTRATOR.md §7). | None — scope discipline. | Explicit at §0 + §4.4. | F5 CH6 fold; prompts are Pass-Omega CRUD only. | ACCEPT. |
| F6 | SPEC §1 non-negotiables Edit F ("V3 real-PMU c/B is a diagnostic characteriser of hot leaves, not a producer; it does not enable any behavior admission path that was blocked in V2"). | SPEC §1 + HANDOFF §5 PMU-producer pre-block class. | Explicit. | Non-weakening clarifier; preserves PMU-as-diagnostic discipline. | ACCEPT. |
| F7 | G-S-P1-RERUN-CONVERGED bar item 14 explicit CH3 enforcement: "No V3 finding silently re-proposes a pre-blocked route (HANDOFF §5 + the class umbrellas from §3.2 of this manifest). CH3 enforces." | All HANDOFF §5 routes + class umbrellas. | Explicit at §5.3 item 14. | This V4 CH3 file IS the F7 enforcement artefact. | ACCEPT. |

---

## §3 — Aggregate verdict

| Report | ACCEPT | WATCH | REVISE | REJECT |
|---|---:|---:|---:|---:|
| P1-V3-A (V4) | 5 | — | — | — |
| P1-V3-B (V4) | 5 | — | — | — |
| P1-V3-C (V4) | 5 (incl. closing V3 C2 WATCH) | — | — | — |
| P1-V3-D (V4) | 8 (incl. closing V3 D1/D3 REVISE + V3 D2 WATCH) | — | — | — |
| P1-V3-E (V4) | 6 | — | — | — |
| P1-V3-F (V4) | 7 | — | — | — |
| **Total** | **36** | **0** | **0** | **0** |

**ACCEPT rate: 36/36 = 100.0%** — clears the PASS-1-PROFILE §4 ≥95% bar
strictly. The V3 → V4 fold closed every V3 REVISE (D1, D3) and every
V3 WATCH (C2 noinline build, D2 W2 framing).

**Net CH3 verdict for V4: FULL ACCEPT.** Zero V4 findings silently
reopen a REDRESS route; the two V3 soft-reopens (D1 string-plane, D3
V10 unicode) close per F3 with explicit REDRESS 60/61/62/64/83/84 +
59/82 cite blocks and same-row-falsification-gate routing. The V3
D-§6.6 wave-prescription strip holds (F1) without leakage into §6.1
or §6.2. D-§6.4 (formerly V3 §6.5) reads "profile not redesign" with
REDRESS 66–69 + 93 binding. E §2 correctly distinguishes the rejected
NEON kernel (REDRESS 28+33) from the admitted scalar (REDRESS 72) with
explicit file:line on both sides. F §2.13's expanded SUPERSEDED
reasoning closes the seven supersession chains with admit SHA
citations + shape-differential prose. D-§6.1 carries the explicit
Lock-1 substrate-cardinality "REPLACES not alongside" binding per F6.
No new REDRESS reopens introduced by the fold; the four typed-GO + 3
direct-GO rows remain explicitly guarded; the four F2 class umbrellas
strengthen the HANDOFF §5 pre-block ledger.

The V4 fold is monotonic with respect to the CH3 lens: it narrows the
cohort posture from "wave proposals with soft REDRESS reopens" to
"diagnostic findings with explicit REDRESS material differentials and
S-P3-routed wave authoring". The two-consecutive-ACCEPT requirement
(PASS-1-PROFILE §4) is half-met by V4; a V5 confirmation cycle remains
required for the gate.

---

## §4 — Remaining REDRESS-regression risks

### §4.1 — S-P3 dispatch risk (medium)

The most material residual risk is **S-P3 misreading the V4 D-§6.1 /
§6.2 findings as wave admission**. The V4 fold demotes intervention
to S-P3 scope via F1, but the diagnostic-finding language ("the gap
on 9 of 11 rows lives inside the delimiter contribution") is
suggestive of a wave shape. Mitigation: F2's four HANDOFF §5 umbrellas
land BEFORE S-P3 dispatches; the umbrellas include the
"string-scanner widening / boundary collapse / per-quartet classifier"
class umbrella that pre-blocks the D1/D3 shapes by reference. If the
umbrellas land cleanly in HANDOFF §5 (F2 prescribes this; the §4.2
Edit E in V4-F is the diff), the residual risk drops to LOW.

### §4.2 — V5 re-CHALLENGE drift risk (low)

The §3W discipline requires "≥95% ACCEPT × 2 consecutive cycles".
V4 alone is one cycle; V5 must re-CHALLENGE without substantive
change. The risk is that V5 reviewers spot a CH3-adjacent defect not
visible to V4 (e.g., a CH2 generality issue that implicates a
REDRESS route). The V4 B fold's primitive-class vocabulary (Lock 14 /
Lock 16) should mitigate this — the substrate-neutral naming
removes the most common CH3-via-CH2 attack surface (JSON-role names
implying JSON-specific routes).

### §4.3 — F2 umbrella adoption risk (low)

The four HANDOFF §5 umbrella additions are *proposed* in V4-F §3.2 +
§4.2 Edit E; they have not yet landed in HANDOFF.md. If they do not
land before S-P3 dispatch, the D1/D3 findings sit naked of the
umbrella-class pre-block. F6 (the V4 CH3-enforcement bar item 14)
binds the gate to "HANDOFF §5 + class umbrellas from §3.2", so the
gate language guards this, but the gate evaluator must verify HANDOFF
§5 carries the umbrellas before signing G-S-P1-RERUN-CONVERGED.

### §4.4 — Per-quartet classifier re-temptation risk (low)

REDRESS 82 retired the per-quartet `\uXXXX` classifier on the same
rows the V4 §6.2 finding names. The risk that a future agent reads §6.2
as licence to re-propose a per-quartet variant is bounded by (a) the
explicit REDRESS 82 cite in §6.2, (b) the F2 umbrella in HANDOFF §5
covering "per-quartet / per-segment unicode-escape classifier routes",
and (c) the V4-B primitive-class vocabulary that names the
`escape_codec_hex_unit` class as substrate-neutral and parameterised
(removing the JSON-specific framing). Triple-locked; residual risk LOW.

### §4.5 — Direct guard-row pressure (low)

V4 D §6.4 reads the direct-plane decorrelation cleanly and routes
direct-plane intervention to a "dedicated direct-output-contract or
control-path tranche" (REDRESS 93). The risk that a future wave
re-proposes direct-source-hook field folding (REDRESS 66) or DirectBuild
semantic-string-fact (REDRESS 69) without that tranche is bounded by
F2's HANDOFF §5 direct-receiver umbrella (REDRESS 66-69). LOW.

### §4.6 — REDRESS-72 scalar admit erosion risk (low)

The SAFE-TO-DELETE NEON kernel deletion (E §2.2) is correctly bounded
to `bbnf-simd/src/aarch64/match_tiny_plain_string.rs` with per-ISA
commit granularity and the explicit "the scalar inside generated.rs is
NOT touched" guard. The only erosion path is if a future cleanup
agent reads E1 as licence to touch
`runtime/src/grammars/json/generated.rs:171-185`. The V4 E §4.3 step 5
commit-granularity guard ("one commit for aarch64 NEON
`match_tiny_plain_string` (NEON kernel only — the scalar in
`runtime/src/grammars/json/generated.rs:171-185` is NOT touched)")
binds this at the commit-message level, so a misread would be
visible in the diff. LOW.

### §4.7 — Cumulative risk verdict

Aggregating §4.1–§4.6: the **only** residual CH3 risk surface above
LOW is **§4.1 S-P3 dispatch misreading**, and that drops to LOW once
F2's umbrellas land in HANDOFF §5. The CH3 lens is therefore on a
glide path to full closure once V5 re-CHALLENGE confirms and HANDOFF
§5 carries the four umbrella additions.

---

## §5 — Summary

- **No CH3 REJECT.** Zero V4 findings silently reopen a REDRESS route.
- **No CH3 REVISE.** Both V3 REVISEs (D1 W1 string-plane, D3 V10
  unicode) close per F3 with explicit REDRESS-60/61/62/64/83/84 +
  REDRESS-59/82 cite blocks and same-row-falsification-gate routing.
- **No CH3 WATCH.** Both V3 WATCHes (C2 noinline build, D2 W2 framing)
  close — C2 by deletion of the probe proposal, D2 by F3 reframing of
  the digest wave as "profile not redesign".
- **F1 wave-prescription strip holds.** V4 D §6.6 reads "wave-class
  authoring belongs to S-P3"; §6.1 / §6.2 stop at diagnostic findings.
- **F6 Lock-1 binding is explicit.** V4 D §6.1 carries "REPLACES not
  alongside" with Lock 1 quoted and file:line citations of both hot-path
  consumers.
- **F2 umbrella additions are correct.** The four HANDOFF §5 class
  umbrellas each cite 3+ binding REDRESS entries; no admit is created.
- **F5 expanded SUPERSEDED reasoning.** Each of the seven entries (35,
  36, 37, 38, 46, 49, 70) now carries multi-line shape-differential
  reasoning with admit SHA citations.
- **NEON vs scalar distinction holds.** E §2.2 explicitly deletes only
  the NEON kernel (REDRESS 28+33) and explicitly preserves the scalar
  (REDRESS 72); per-ISA-family commit granularity bounds the blast
  radius.
- **No new REDRESS reopens introduced by the V3 → V4 fold.** The fold
  is monotonic.
- **The 4 typed-GO + 3 direct-GO admitted rows are explicitly guarded**
  (V4 D §6.3 / §6.4 / §6.5).
- **Substrate cardinality (Lock 1 / SC-6) is satisfied.** V4 D §6.1's
  Lock-1 binding sentence is explicit; F6 holds.

Net CH3 verdict for V4: **FULL ACCEPT at 36/36 = 100%.** The V4 fold
clears the CH3 lens cleanly. The two-consecutive-cycle requirement
remains the only gate barrier; V5 re-CHALLENGE without substantive
change is the next move per PASS-1-PROFILE §4. The four F2 umbrellas
should land in HANDOFF §5 before any S-P3 dispatch as a defense in
depth against §4.1.
