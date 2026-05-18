# SK-V9 S-P3 Hardening — CH1 CORRECTNESS — V1

Lens: CH1 CORRECTNESS. Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-18.
Cohort under review: `research/p3/skv9-p3-{A,B,C,D,E}-*.md` +
`skv9-p3-F-spec-draft.md` + `skv9-p3-F-dispatch-draft.md` (seven
artefacts).
Convergence rule: per `ORCHESTRATOR.md` §3W + §3Z, S-P3 must clear
≥95% × 2 consecutive cycles.

---

## §1 — Method

CH1 adversarially verifies factual correctness: every cited file:line,
REDRESS / RESULTS / commit anchor resolves; cross-artefact figures and
manifests agree; arithmetic is internally consistent; the P3-F draft
(authored before its siblings) is reconciled against P3-A/B/C/D/E.

Verification performed against the live tree:

- **Code spot-checks (≥15):** `gate.rs` `enum Outcome` (15 variants);
  `gate.rs:56` `DIRECT_PROJECTION_SONIC_SLACK`; `gate.rs` verdict map;
  `report.rs:8-9` `SCHEMA_V3_HEADER`/`_ALIGN`; `report.rs:977-989`
  `validate_w0_outcome`; `report.rs:685-695`
  `SK_V9_OPEN_RUN_ID_PREFIX`/`is_skv9_open_run_id`; `report.rs:709`
  `SK_V8_OPEN_BASELINE`; `report.rs:33-67` `SkV8ComparatorEvidence` /
  `SkV8Telemetry`; `report.rs:220/276/499/509` `validate_schema_v3` /
  `validate_sk_v8_w0`; `bin/gate.rs:1825-1831` regression test
  `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures`;
  `bin/gate.rs:1199` `w0_real_typed_metadata_expected`;
  `parse-that-regex/src/lib.rs:402` `unescape_uxxxx_x4_neon` call site;
  `lib.rs:162` `match_string_at_quote_trusted_utf8`; `lib.rs:718`
  `unescape_string`.
- **RESULTS spot-checks:** twitter / apache_builds / gsoc-2018 /
  distinct_values / update_center / canada / citm_catalog / numbers /
  marine_ik / mesh / instruments parse_only Track 1 + sonic-strict.
- **REDRESS spot-checks:** the W10b six-row regression block
  (`REDRESS.md:2573-2585`), the SK-V8 W2 source/product-parity verdict
  (`:2651-2658`), the SK-V8 W3 routed-precursor text (`:2688-2691`).
- **Cross-artefact:** P3-A 8-candidate shortlist vs P3-B 4-wave
  manifest vs P3-C 6-wave gate table vs P3-E 6-wave ledger vs P3-F
  7-wave SPEC manifest.

The cohort is largely accurate at the citation level — the load-bearing
code claims (the outcome-enum discrepancy, the slack constant, the
wired x4 codec path, the regression test flip) all resolve correctly.
The defects are concentrated in (a) one fabricated/stale sonic-strict
floor propagated through three artefacts, and (b) a structural
wave-count and wave-name divergence between P3-F and every sibling.

---

## §2 — Disposition table

| # | Claim under review | Artefact | Verdict | Evidence |
|--:|---|---|---|---|
| 1 | `gate::Outcome` defines 15 variants `A B C D E F-positive F-noise G I J K L M N-direct S` | P3-D §3.1 | ACCEPT | `gate.rs:4-21` — exactly 15 variants; `id()` table at `:96-131` confirms the string set. |
| 2 | `validate_w0_outcome` restricts W0-admissible set to 10: `A C G I J K L M N-direct S` | P3-D §3.1 | ACCEPT | `report.rs:977-989` `matches!(outcome_id, "A"\|"C"\|"G"\|"I"\|"J"\|"K"\|"L"\|"M"\|"N-direct"\|"S")` — exactly 10. Cited as `977-988`; actual `977-989` (off-by-one, trivial). |
| 3 | SPEC §0.3 names a 7-outcome enum `A C G K L N-direct S`, omitting `I`/`J`/`M` | P3-D §3.1 | ACCEPT | Current `SPEC.md` §0.3 carries no explicit outcome list (it is the *placeholder* recovery SPEC); P3-D is reading the P3-F-drafted §0.3. The discrepancy P3-D names — code admits 10, P3-F §0.3 implies 7 — is real: see #4. Verdict ACCEPT for the *finding*; the SPEC-text it indicts is P3-F's draft, not the live SPEC. |
| 4 | P3-D ruling: SK-V9 outcome enum must be the 10-identifier set, SPEC §0.3 corrected accordingly | P3-D §3.2 | ACCEPT | The ruling is arithmetically forced: `validate_w0_outcome` gate-admits 10; a 7-identifier SPEC enum would make `gate-json` reject a row the validator itself admits. P3-F §0.3 as drafted does NOT enumerate outcomes at all — it lists row *families*, not the outcome set. So P3-D's "correct the SPEC §0.3 list" instruction has no target in the current P3-F draft. **V2 fold:** P3-F §0.3 must add the explicit 10-identifier outcome enum P3-D rules; today it is absent, so the producer/consumer contradiction P3-D guards against is unresolved in the draft. |
| 5 | `DIRECT_PROJECTION_SONIC_SLACK = 1.10` at `gate.rs:56` | P3-A C1, P3-C §1.2 | ACCEPT | `gate.rs:56` `pub const DIRECT_PROJECTION_SONIC_SLACK: f64 = 1.10;` — exact. |
| 6 | Regression test `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures` at `gate.rs:1820-1831` asserts twitter/update_center true, apache/citm false | P3-A C1, P3-F §4 | ACCEPT | `bin/gate.rs:1825-1831`; asserts `w0_real_typed_metadata_expected` true for twitter/update_center, `!`-false for apache_builds/citm_catalog. Cited `1820-1831`; `#[test]` at `1825`. Trivial line drift; substance exact. |
| 7 | `unescape_uxxxx_x4_neon` is already wired in production at `parse-that-regex/src/lib.rs:402` | P3-A C4, P3-E §3.4 | ACCEPT | `lib.rs:401-402` calls `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon(&packed)` — the x4 path is live; the P2-D §0 "not wired" V1 correction holds. |
| 8 | `match_string_at_quote_trusted_utf8` at `lib.rs:162` is the C5 same-wave consumer | P3-A C5, P3-C W4 | ACCEPT | `lib.rs:162` `pub fn match_string_at_quote_trusted_utf8(input, offset)` — exact. |
| 9 | P3-D cites the P2-E JSON production consumer as `unescape_string` at `lib.rs:718` | P3-D §2.3, §7 | ACCEPT | `lib.rs:718` `pub fn unescape_string(...)` — exact. (Note P3-A C4 names the consumer as the x4 path at `:402`; both are correct — `:402` is inside the `:718` materialiser's call tree. Not a contradiction.) |
| 10 | `SK_V9_OPEN_RUN_ID_PREFIX` / `is_skv9_open_run_id` at `report.rs:685-695` | P3-D §6.1 | ACCEPT | `report.rs:685` `pub const SK_V9_OPEN_RUN_ID_PREFIX = "sk-v9-open:criterion-fnv64-"`; `is_skv9_open_run_id` at `687`. Exact. |
| 11 | `SCHEMA_V3_HEADER`/`SCHEMA_V3_ALIGN` at `report.rs:8-9` | P3-D §2.1, §6.1 | ACCEPT | `report.rs:8` `SCHEMA_V3_HEADER`, `:9` `SCHEMA_V3_ALIGN` — exact. |
| 12 | Schema-v3 is 26 columns | P3-D §2.1 | REVISE | `SCHEMA_V3_HEADER` literal has 26 `\|`-delimited fields. P3-D states "26 schema-v3 columns + 22 manifest columns, 31 distinct gate-consumed identifiers" but its own §2.2 table runs to **36 rows** and the prose says "the canonical gate-consumed identifier set is the 36-row table." The 26 / 22 / 31 / 36 numbers are not reconciled — §2.1 says 31, §2.2 says 36, the SPEC §0.4 list (verified) has 35 entries. The arithmetic (26+22 with overlaps → 36; SPEC names 31) is plausible but P3-D never shows the overlap subtraction. **V2 fold:** state the 26 ∪ 22 → 36 derivation explicitly with the overlapping identifiers named. |
| 13 | `validate_w0_outcome` is at `report.rs:977-988` | P3-D §1, §3.1 | REVISE | Actual `977-989`. Off-by-one. Cosmetic but a CH1 line-citation defect. |
| 14 | RESULTS twitter parse_only Track 1 = 13188 | P3-A §2.2 C3, P3-C W3 | ACCEPT | `RESULTS.md` twitter/parse_only col 10 = 13188. Exact. |
| 15 | RESULTS apache_builds parse_only Track 1 = 11917 | P3-A C3, P3-C W3 | ACCEPT | apache_builds/parse_only col 10 = 11917. Exact. |
| 16 | RESULTS gsoc-2018 parse_only Track 1 = 22184 | P3-A C3/C5, P3-C W3/W5 | ACCEPT | gsoc-2018/parse_only col 10 = 22184; sonic-strict col 12 = 45318. Both exact (P3-C §5 exit gate `41198 = 45318/1.10` arithmetically correct). |
| 17 | RESULTS distinct_values parse_only Track 1 = 8972 | P3-A C3, P3-C W3 | ACCEPT | distinct_values/parse_only col 10 = 8972. Exact. |
| 18 | RESULTS update_center parse_only Track 1 = 9857 | P3-A C3, P3-C W3 | ACCEPT | update_center/parse_only col 10 = 9857. Exact. |
| 19 | W10b maintain floor `citm_catalog ≥ 28631` | P3-A §2.2, P3-C W3, P3-F §6 | ACCEPT | citm_catalog/parse_only Track 1 = 29215; `29215 × 0.98 = 28631`. Exact. |
| 20 | W10b maintain floor `numbers ≥ 17597` | P3-A §2.2, P3-C W3, P3-F §6 | ACCEPT | numbers/parse_only Track 1 = 17956; `17956 × 0.98 = 17597`. Exact. |
| 21 | W10b maintain floor `marine_ik ≥ 11831` | P3-A §2.2, P3-C W3, P3-F §6 | ACCEPT | marine_ik/parse_only Track 1 = 12073; `12073 × 0.98 = 11831`. Exact. |
| 22 | W10b maintain floor `mesh ≥ 12186` | P3-A §2.2, P3-C W3, P3-F §6 | ACCEPT | mesh/parse_only Track 1 = 12435; `12435 × 0.98 = 12186`. Exact. |
| 23 | W10b maintain floor `canada ≥ 15866` (today×0.98) and "sonic floor 15871 binds higher" (P3-A) / `canada ≥ 15871` (P3-F §6) | P3-A §2.2, P3-C W3, P3-F §6 | **REJECT** | canada/parse_only Track 1 = **16190** → `16190 × 0.98 = 15866` ✓ for the today×0.98 leg. But canada parse_only **sonic-strict = 12723**, NOT 15871. The "sonic floor 15871 binds higher" claim (P3-A §2.2) is false — 12723 < 15866, so today×0.98 binds. P3-F §6 exit gate states `canada ≥ 15871` outright, citing a non-existent sonic figure as the binding floor. The cited 15871 is fabricated or stale (SK-V8-era). The *binding* floor (15866) happens to be correct via the "whichever higher" clause, but the stated gate number in P3-F is wrong. |
| 24 | W10b six-row regression: `canada` T1 −3.11%/T2 −4.14%; `citm_catalog` T1 −7.36%; `instruments` T1 −3.96%; `marine_ik` T1 −5.68%; `mesh` T1 −8.07%/T2 −7.46%; `numbers` T1 −6.44% | P3-E §5 | ACCEPT | `REDRESS.md:2573-2585` — every percentage verbatim. Exact. |
| 25 | REDRESS 91: "W2 admits source/product parity only and does not claim six measured `real_typed_struct A / GO` rows" | P3-A C1, P3-E §3.1 | ACCEPT | `REDRESS.md:2651-2653` verbatim. P3-E §3.1's longer quote also matches `:2654-2658`. |
| 26 | REDRESS 92 routed precursor: "define the retained class/event grammar … prove the retained `ValueRef` cursor contract … only then reopen a measured structural-heavy parse row wave" | P3-A C2/C3, P3-E §3.2, P3-F §5 | ACCEPT | `REDRESS.md:2688-2691` verbatim. |
| 27 | S-P2 converged 6/6 ≥95%; S-P1 converged 6/6 | P3-A §1, P3-B §1, P3-F status | ACCEPT | `HARDENING-S-P2-CONVERGED.md` audit table — all six lenses two consecutive ≥95%; `HARDENING-S-P1-CONVERGED.md` likewise. |
| 28 | W0 closed at run-id `sk-v9-open:criterion-fnv64-cd1673844eeea12f` | all | ACCEPT | Matches `report.rs:685` prefix + `HANDOFF.md:42` + `SPEC.md:33`. |
| 29 | W0 closed at commit `90609aee` | P3-B §1, §2 | REVISE | P3-B asserts commit `90609aee`; no other artefact cites a commit hash for W0, and the SK-V9 SPEC/HANDOFF name the close *artefact* (`skv9-W0-close.md`) but no SHA. Not verifiable from the provided inputs — `git log` was not cross-checked for this hash in-cohort. Flag for V2: either substantiate the SHA or cite the close artefact instead. |
| 30 | P3-A 8-candidate shortlist (C1-C8) vs P3-B 4-candidate post-W0 pool | P3-A §2, P3-B §1 | REVISE | Not a contradiction but an unreconciled granularity mismatch: P3-A enumerates **8 candidates**; P3-B collapses them to **4** ("aarch64 ASM consumers + unicode codec + string-block widening" = P3-A's C4+C5+C6+C7+C8 as one row). Both are internally coherent and P3-A §1.1 explains the fold, but neither artefact states the 8↔4 mapping as a table. A reader of P3-B alone cannot recover C6/C7/C8. **V2 fold:** add the explicit 8-candidate → wave map. |
| 31 | Wave **count** across the cohort | P3-A, P3-B, P3-C, P3-E, P3-F | **REJECT** | Four different counts. P3-B: **W1–W5** (5 behavior waves, W5=close). P3-C §1.4: **W0–W5** with **W4 = codec+string-block paired**, **W5 = aarch64 ASM kernels** (codec and ASM in *separate* waves). P3-E: **W0 + W-AC/W-RG/W-UE/W-UC/W-AS** (6 letters, W-UC = codec+string-block+ASM merged). P3-F: **W0–W6** (7 waves; W4=string-block, **W5=codec**, W6=close). The cohort does not agree on how many waves exist or what each contains. |
| 32 | Candidate-to-wave **mapping** — the codec (P2-E) and string-block widening (P2-D §4) | P3-B, P3-C, P3-F | **REJECT** | P3-B W4 and P3-C §4.3 both bind the codec + 32-byte string-block widening **into one wave** ("must be one wave, not two" — P3-C §4.3 is explicit and load-bearing for the non-vacuous exit gate). P3-F **splits them**: §7 W4 = string-block widening alone, §8 W5 = codec alone, with W5 "paired with the W4 string-scanner widening." P3-F's split directly contradicts P3-C §4.3's central honesty argument — under P3-F's split, W4 (string-block alone) and W5 (codec alone) each face the paper-close P3-C §4.3 forbids. This is the cohort's most material divergence. |
| 33 | Wave **names / SPEC-section binding** | P3-B §2 note, P3-F §2 | **REJECT** | P3-B §2 note explicitly says "P3-F resolves the SPEC-slot rename: the SPEC's W1 release slot is consumed by S-P3 convergence … behavior waves shift to occupy SPEC §6/§7/§8/§9." P3-F does NOT do this — P3-F §2 manifest binds W1→§4, W2→§5, W3→§6, W4→§7, W5→§8, W6→§9, with no "release" wave consumed. P3-B's stated expectation of P3-F is unmet; the two artefacts describe incompatible section maps. |
| 34 | P3-F §0.2 goalset table: diagnosis #2 (string-scanner) → "Owning wave W4"; #3 (codec) → "W5"; #4 → "W4 + W5" | P3-F §0.2 | REJECT (consequence of #32) | Internally consistent with P3-F's own split manifest, but inconsistent with P3-C's pairing and P3-A §2.1 (which sequences codec C4 + string-block C5 same-wave). The goalset table propagates the #32 defect into §0. |
| 35 | P3-F §0.4 adds three new required telemetry fields (`checkasm_parity_status`, `union_class_column_status`, `codec_admission_basis`) | P3-F §0.4 | **REJECT** | P3-F §0.4 says SK-V9 behavior waves "add three required fields." P3-D §2.1 is categorical and opposite: "The W0-frozen schema is carried forward **unchanged** … **No behaviour wave adds a column** … No SK-V9 wave adds a 37th." P3-F adding three columns directly violates P3-D's binding ruling and P3-F's own §1 non-negotiable cohesion. The §0.4 `[INTEGRATE P3-D]` marker flags it, but the contradiction is substantive: P3-D forbids exactly what P3-F drafts. |
| 36 | P3-F §6 W3 exit gate omits `distinct_values` | P3-F §6, P3-A §2.2, P3-C W3 | REVISE | P3-A §2.2 C3 and P3-C W3 both name `distinct_values ≥ 15731` as a W3 must-improve exit row. P3-F §6 G-W3-UNION-SUBSTRATE clause 1 lists only twitter/apache_builds/update_center — `distinct_values` dropped. Either an omission or an intentional narrowing; P3-F does not say which. **V2 fold:** restore `distinct_values` or justify its absence. |
| 37 | P3-C §1.4 candidate→wave map places "P2-D §5 aarch64 ASM kernels" in W5 as row-moving for `github_events`/`random` | P3-C §1.4, §5 | REVISE | P3-C invents W5 exit-gate rows `github_events ≥ 19418` and `random ≥ 13788` not present in P3-A's shortlist (P3-A C6/C7 are explicitly "no row of their own"). P3-C §5 derives `github_events` from "sonic 21360 / 1.10" and `random` from "sonic 15166 / 1.10" — these sonic figures were not RESULTS-spot-checked in this review and are not cross-cited by any sibling. **V2 fold:** confirm the W5 row Mbps against RESULTS or remove the un-sourced exit rows. |
| 38 | P3-F §8 W5 owner path: "`codegen/src/` codec template (single file)" | P3-F §8 | REVISE | P3-A C4 specifies `codegen/src/escape_codec/` as a **new sub-module** (mod.rs + per-binding files), consistent with the `feedback_module_structure` / `no-god-modules` discipline. P3-F §8 collapses it to "a single file." Divergence from P3-A; the single-file form risks a god-module. |
| 39 | P3-E §1 REDRESS reconciliation: 93 entries, 7 SUPERSEDED (35,36,37,38,46,49,70), ~60 load-bearing | P3-E §1, §6 | ACCEPT (unverified count) | `REDRESS.md` uses prose section headers, not numbered entries; the "93 entries" / "7 SUPERSEDED" figures are inherited from `skv9-p1-v3-F-redress-reconciliation.md` (cited) and not independently re-counted here. Internally consistent across P3-E; no contradiction found. Carried as ACCEPT on cited authority. |
| 40 | P3-D §2.2 field table runs #1-#36 but the column header says "31 required identifiers" | P3-D §2.2 | REVISE | The table is numbered 1-36 yet the prose lead-in says "The 31 required identifiers." Same unreconciled 31/36 confusion as #12. Cosmetic-to-substantive: a reader cannot tell whether the gate requires 31 or 36 fields. |
| 41 | P3-F dispatch-draft cites the pre-block ledger as `research/alpha/alpha-C-redress-digest.md` | P3-F dispatch §"Pre-Blocked Routes" | REVISE | The SK-V9 HANDOFF §5 also cites this path as "binding by reference," so P3-F is consistent with HANDOFF. But P3-E is the *new* S-P3 pre-block ledger and supersedes the alpha digest; the dispatch draft should cite P3-E (it carries an `[INTEGRATE P3-E]` marker, so this is flagged, not hidden). Minor. |
| 42 | P3-B §2 / P3-F: W0 entry-gate `G-S-P1-RERUN-CONVERGED` + `G-BEHAVIOR-RELEASE` both treated as satisfied | P3-B §2, P3-F dispatch-lock | ACCEPT | `HARDENING-S-P1-CONVERGED.md` records the S-P1 rerun converged; P3-B §2 correctly notes `G-BEHAVIOR-RELEASE` = S-P3 convergence itself. Consistent with `SPEC.md` §5 and the converged docs. |
| 43 | P3-A C2 LOC envelope ~395-425 vs HANDOFF 450 cap | P3-A C2, HANDOFF §3 | ACCEPT | HANDOFF §3 binds "Retained class/event grammar plus `ValueRef` proof — 450" LOC. P3-A C2 ~395/~425 is inside it. P3-F §2 manifest states "≤425 hand" — consistent. |
| 44 | P3-A C1 LOC ~300 vs HANDOFF 300 cap | P3-A C1, HANDOFF §3, P3-F §2 | ACCEPT | HANDOFF §3 "Apache/CITM typed row-table admission — 300." P3-A ~300, P3-F §2 "≤300 hand." Consistent. |
| 45 | P3-F §2 W5 budget "≤600 hand + ≤120 regen" | P3-F §2, P3-A C4 | REVISE | P3-A C4 sizes the codec at **~1,045 net LOC** (P2-E §7.4), the largest candidate. P3-F §2 W5 budget is "≤600 hand." If W5 = codec alone (P3-F's split), 600 < 1,045 — the budget under-provisions the candidate by ~440 LOC. HANDOFF §3 has no codec line (the codec is a P2-E artefact post-dating HANDOFF). **V2 fold:** reconcile the W5 LOC budget with P2-E §7.4's 1,045 figure, or re-pair per #32 so the budget covers the actual slice set. |
| 46 | P3-C W4 exit-gate thresholds `unicode_escapes ≥ 16319`, `y_string_unicode ≥ 8270`, `unicode_mixed ≥ 12338` arithmetic | P3-C §4.1 | ACCEPT | `16319 = ceil(18132 × 0.90)`; `8270 = ceil(11814 × 0.70)`; `12338 = ceil(14515 × 0.85)`. All three arithmetically consistent with the stated sonic-strict bases and slack rules. (The sonic-strict bases 18132/11814/14515 themselves are P2-E-sourced and not RESULTS-spot-checked here — carried on P2-E authority.) |
| 47 | P3-C §4.1 projection table: `unicode_escapes` baseline 11239 vs P3-A C4 "baseline 21,646" for gsoc / P2-E figures | P3-C §4.1, P3-A C4 | ACCEPT | P3-C §4.1 and P3-A C4 agree row-for-row on the four-row projection (15423/7837/7864 projected; NEAR-FAIL 94.5/94.8, FAIL 63.7%). Internally consistent across P3-A and P3-C. |
| 48 | P3-B §4 verdict: every SK-V9 wave dispatches under SK-V9 authority, no Pass Omega gate | P3-B §4 | ACCEPT | Consistent with HANDOFF §3 ("Pass Omega owns SC-6-L1-R1 …") and P2-B §5 (SC-6-L1-R1 not pre-bound). No contradiction. |

---

## §3 — Aggregate verdict

48 dispositions: **30 ACCEPT, 13 REVISE, 5 REJECT.**

ACCEPT rate = 30 / 48 = **62.5%.**

This is **far below** the §3Z 95% threshold. The cohort does not
converge on CH1 cycle V1.

The five REJECTs are not citation typos — they are substantive
correctness defects:

- **#23** — a fabricated/stale sonic-strict floor (`canada 15871`)
  propagated through P3-A, P3-C, and P3-F; the live RESULTS sonic-strict
  for canada parse_only is 12723.
- **#31, #32, #33, #34** — a four-way structural disagreement on wave
  count, candidate-to-wave mapping, and SPEC-section binding. P3-F
  (drafted before its siblings) splits the codec and string-block
  widening into separate waves, directly contradicting P3-C §4.3's
  load-bearing pairing argument, and does not perform the SPEC-slot
  rename P3-B §2 explicitly delegates to it.
- **#35** — P3-F §0.4 adds three telemetry columns that P3-D §2.1
  categorically forbids ("no behaviour wave adds a column").

The cohort's citation-level accuracy is high (the code anchors, the
REDRESS verbatims, the regression test, the slack constant, the wired
codec path all resolve), but the *synthesis* layer — the wave plan that
is the entire point of S-P3 — is internally incoherent because P3-F was
authored against the S-P2 evidence alone and its `[INTEGRATE P3-x]`
markers were never resolved.

---

## §4 — Defects requiring V2 fold

**REJECT-class (must fix before V2 can score):**

1. **#23 — the `canada` W10b floor.** Replace the fabricated sonic
   figure. canada parse_only Track 1 = 16190, sonic-strict = 12723; the
   binding floor is `today × 0.98 = 15866`. Correct P3-A §2.2, P3-C W3,
   and P3-F §6 (`canada ≥ 15871` → `canada ≥ 15866`). Audit every other
   "sonic floor binds higher" claim in the cohort against live RESULTS
   sonic-strict columns — the canada error suggests the maintain-block
   sonic figures were carried from an SK-V8-era report.

2. **#31/#32/#33/#34 — wave plan reconciliation.** The cohort must
   converge on ONE wave manifest. The substantive question is whether
   the codec (P2-E) and the 32-byte string-block widening (P2-D §4)
   share a wave. P3-C §4.3 argues — convincingly, with the
   non-vacuous-exit-gate proof — that they MUST. P3-F splits them and
   thereby reintroduces the paper-close P3-C forbids. **V2 must adopt
   the P3-C pairing and re-fold P3-F §7/§8 into a single paired wave**,
   then re-bind the SPEC sections and re-state the goalset table
   (P3-F §0.2). P3-B's §2 "P3-F resolves the SPEC-slot rename"
   expectation must either be performed in P3-F or struck from P3-B.

3. **#35 — telemetry column addition.** P3-F §0.4's three new required
   fields contradict P3-D §2.1's binding "no new column" ruling. Either
   P3-D must be amended to admit the three fields (and show how
   `validate_schema_v3`/`validate_sk_v8_w0` consume them in-wave), or
   P3-F §0.4 must drop them and carry the W0 schema unchanged. P3-D's
   position is the stronger one (it is grounded in the code's
   `SCHEMA_V3_HEADER` literal and the same-wave-consumption rule); V2
   should fold P3-F §0.4 to match P3-D unless a consumption path for the
   three fields is demonstrated.

**REVISE-class (correct, do not block scoring on their own):**

4. **#4** — P3-F §0.3 must add the explicit 10-identifier outcome enum
   P3-D §3.2 rules; the current §0.3 omits an outcome list entirely, so
   P3-D's "correct the SPEC list" instruction has no target.

5. **#12/#40** — P3-D must reconcile the 26 / 22 / 31 / 36 column-count
   arithmetic; show the 26∪22→36 overlap derivation and use one
   consistent number in the §2.2 lead-in.

6. **#13** — `validate_w0_outcome` line citation `977-988` → `977-989`.

7. **#29** — substantiate the W0 close commit `90609aee` (P3-B) or cite
   the close artefact instead.

8. **#30** — add the explicit P3-A 8-candidate → P3-B/P3-F wave map so
   C6/C7/C8 are recoverable from the wave manifest.

9. **#36** — P3-F §6 W3 exit gate dropped `distinct_values` (a P3-A/P3-C
   must-improve row); restore or justify.

10. **#37** — P3-C §5 W5 exit rows `github_events ≥ 19418`,
    `random ≥ 13788` are un-sourced against RESULTS; verify or remove.

11. **#38** — P3-F §8 codec template "single file" contradicts P3-A C4's
    `codegen/src/escape_codec/` sub-module; align to the sub-module form
    (no-god-module discipline).

12. **#41** — P3-F dispatch-draft should cite P3-E as the live
    pre-block ledger, not the superseded `alpha-C-redress-digest.md`.

13. **#45** — P3-F §2 W5 LOC budget (≤600) under-provisions the codec
    (~1,045 net per P2-E §7.4); reconcile with the re-paired wave from
    defect #2.

V2 cannot be scored against the §3Z bar until the five REJECT-class
defects (items 1-3 above) are folded. The REVISE-class items should be
folded in the same pass.
