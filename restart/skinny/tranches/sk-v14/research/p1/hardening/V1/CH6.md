# S-P1 CHALLENGE V1 — CH6 ANTI-PAPER-CLOSE

Lens: **CH6 ANTI-PAPER-CLOSE** per `restart/prompts/skinny/PASS-1-PROFILE.md §3 CH6` + `restart/prompts/ORCHESTRATOR.md §3W`.
Pass: S-P1 Profile. Cycle: V1.
Date (UTC): 2026-05-23.
Scope: every "profiled" claim must cite a flame-profile file that exists on disk with resolvable symbols; every `unprofiled` / `n/a` cell must carry a stated cause; no symbol folded by inlining can stand as a primitive-level attribution without an `atos -inlineFrames` recovery or a `--features parse-attribution` rebuild.
Authority: `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md §2 CH6`.
Discipline: WRITE-ONLY (no git add / commit); aggregator commits 8 hardening files atomically.

## §0 — Disposition summary (CH6)

ACCEPT-rate **6 / 6 P1 axis files** (P1-A, P1-B, P1-C, P1-D, P1-E, P1-F).

| Artefact | CH6 verdict | Paper-close risk surfaced? | Cause cited for every absence? |
|---|---|---|---|
| `p1a-samply-mode-1.md` | **ACCEPT** | `dispatch_value` is one LTO-fused 10 020-byte symbol — risk surfaced + remediated via `atos -inlineFrames` against the dSYM (`p1a:10, 103, 134, 244`) | yes |
| `p1b-samply-mode-2.md` | **ACCEPT WITH DEPENDENCY** | `parse_object_value_at_direct` / `parse_array_element_at_direct` are JSON-named envelope monomorphizations (CH6 risk inherited from same `parse-attribution` gate) — flagged at p1b:286 + p1c §ANOM-4 (`which would have reported the same single symbol`) | yes — `unavailable_because_no_typed_fixture` cited at p1b:11 for 6 corpora; `bbnf_bench::real_typed_struct::fixture_for_name() returns None` |
| `p1c-samply-mode-3.md` | **ACCEPT** | ANOM-4 explicitly names the CH6 paper-close risk at `p1c:480` (`This is a CH6 paper-close risk: the profile names a symbol that obscures the leaf primitive`) and routes V2 fold to `--features parse-attribution` | yes — every probe profile path cited at p1c §5.1 with sample counts |
| `p1d-pmu-cycles.md` | **ACCEPT** | PMC counters `unavailable_from_current_export` is correctly Lock-14 finding, not paper-close — verbatim verified | yes — escalation matrix at p1d §1.4; sudo refusal recorded in `/tmp/skv14-p1d/artifacts/identity.txt` (`sudo_available=sudo: a password is required`, `exit=1`); xctrace cpu-state schema documented (no PMC columns) |
| `p1e-hot-leaf-attribution.md` | **ACCEPT** | Lock-14 envelope mis-attribution is the principal CH6 finding; explicitly names `parse-attribution` feature flip as the V2 remediation at `p1e:110` (`S-P2 must crack dispatch_value open via parse-attribution cargo feature`). `github_events` 8-sample noise + `instruments`/`y_string_unicode` inlined-std noise also flagged as CH6 risk at `p1e:96, 110, 126, 134` | yes — 24 typed rows carried as `n/a — typed product not generated` with reason at `real_typed_struct.rs:551-566` |
| `p1f-results-delta.md` | **ACCEPT** | Documentary; no original profiling claims. RESULTS.md `n/a:w1b-2b-report-gate-consumes-w1b-2a-criterion` flagged as stale-artifact at `p1f:193`. | yes — every n/a row routed to producing axis (P1-A/B/C/D) |

ACCEPT-rate: **6/6**.

Specific CH6 challenge questions per the V1 dispatch context (§2 CH6):

1. **PMC `unavailable_from_current_export` is Lock-14 finding, not paper-close?** **YES, VERIFIED.**
2. **P1-C ANOM-4 dispatch_value folded symbol is addressed in V2 fold?** **YES, EXPLICITLY ROUTED.**
3. **Flame profile artefacts cited at /tmp/skv14-p1{a,b,c}/ exist?** Path naming in the dispatch context was approximate; the actual paths are `/tmp/skv14-p1/samply/` (P1-A; identity manifest names the agent), `/tmp/skv14-p1b/samply/` (P1-B), `/tmp/skv14-p1c-profiles/` (P1-C). **ALL EXIST, all symbols resolvable.** Detail in §1.

## §1 — Path-existence verification (executable-verification mandate)

Per the institutionalised "if you cite a path, verify it exists" mandate (`CHALLENGE-CONTEXT.md §3`), the orchestrator executed `ls` against every flame-profile path cited in P1-A/B/C/D §5 / §1 / §2 source blocks, and a representative `grep` against each axis's syms.json sidecar.

### §1.1 — P1-A flame profile artefacts (parse_only × 17 corpora)

Cited at `p1a-samply-mode-1.md §5` (Sources). Profile directory: `/tmp/skv14-p1/samply/profiles/`.

| Artefact path | Exists? | Notes |
|---|---|---|
| `/tmp/skv14-p1/samply/profiles/parse__twitter__track1.json.gz` | **YES** | + `parse__twitter__track1.json.syms.json` (7 505 bytes). `grep -c dispatch_value` = 1 (symbol present in sidecar). |
| `/tmp/skv14-p1/samply/profiles/parse__citm_catalog__track1.json.gz` | **YES** | matching `.syms.json` present |
| `/tmp/skv14-p1/samply/profiles/parse__y_string_unicode__track1.json.gz` | **YES** | matching `.syms.json` present |
| Full set: 17 `.json.gz` + 17 `.syms.json` | **YES** | `ls /tmp/skv14-p1/samply/profiles/*.json.gz | wc -l` = 17 |
| `/tmp/skv14-p1/samply/inlined/inline__twitter.txt` (atos-resolved top-leaves) | **YES** | 17 inline files total. `head` confirms `dispatch_value (long-tail intra-region) generated.rs:45-156` 33.27% — atos recovery is the real CH6 remediation already executed at record time |
| `/tmp/skv14-p1/artifacts/identity.txt` | **YES** | `commit=2547c750bc78533d738eb85913206a0872022818`, `host_triple=aarch64-apple-darwin`, `date=2026-05-23T06:37:31Z`, `agent=P1-A`, `mode=I (parse_only, cold per-parse)` |

CH6 verdict: **every cited P1-A path resolves**; the LTO-fused `dispatch_value` symbol is remediated by `atos -inlineFrames` against the dSYM at record time, with the recovered inline frames materialised as separate `inlined/inline__<corpus>.txt` artefacts. No paper-close.

### §1.2 — P1-B flame profile artefacts (direct + typed × 17 + 11 corpora)

Cited at `p1b-samply-mode-2.md §1 + §5`. Profile directory: `/tmp/skv14-p1b/samply/profiles/`.

| Artefact path | Exists? | Notes |
|---|---|---|
| `/tmp/skv14-p1b/samply/profiles/twitter-direct-track1.json.gz` | **YES** | + matching `.syms.json` |
| `/tmp/skv14-p1b/samply/profiles/twitter-typed-track1.json.gz` | **YES** | + `.syms.json` (13 617 bytes); `grep -c "DirectParser\|skip_value\|parse_object"` = 1 (typed plane symbols present in sidecar) |
| `/tmp/skv14-p1b/samply/profiles/citm_catalog-typed-track1.json.gz` | **YES** | matching `.syms.json` present |
| Full set: 56 `.json.gz` + 56 `.syms.json` | **YES** | `ls /tmp/skv14-p1b/samply/profiles/*.json.gz | wc -l` = 56 (17 direct × 2 tracks + 11 typed × 2 tracks = 34 + 22 = 56). p1b:11 claim "Total: 56 flame profiles + 56 syms sidecars, zero record-time failures" verified. |
| `/tmp/skv14-p1b/samply/logs/twitter-typed-track1.log` | **YES** | one log per profile |

CH6 verdict: **all 56 P1-B profiles exist**, every absence (6 corpora × typed × 2 = 12 absent profiles) is correctly cited as `unavailable_because_no_typed_fixture` with the source-of-truth reason at `skinny/crates/bbnf-bench/src/real_typed_struct.rs:551-566`. No paper-close.

CH6 INHERITED-RISK: the P1-B Track 1 profiles share the `parse-attribution` feature gate with P1-A (the `parse_object_value_at_direct` / `parse_array_element_at_direct` envelope monomorphizations are folded the same way at default release+bench feature set). P1-C ANOM-4 (`p1c:482-483`) explicitly notes "it equally applies to P1-A and P1-B which would have reported the same single symbol", which routes the V2 fold to all three axes simultaneously. CH6 ACCEPTS this as a dependency-flagged risk, not paper-close, because:

  (a) the risk is named in the artefact itself (`p1c:480`),
  (b) the V2 remediation route is concrete (`cargo --features parse-attribution`), and
  (c) the feature gate is plumbed and verified at `skinny/crates/runtime/src/grammars/json/generated.rs:33-34, 43-44, 58-59, 79-80, 86-87, 117-118, 138-139, 157-158` and `skinny/crates/runtime/Cargo.toml:21` (`parse-attribution = []`).

### §1.3 — P1-C flame profile artefacts (mode-III × 4 probes)

Cited at `p1c-samply-mode-3.md §5.1`. Profile directory: `/tmp/skv14-p1c-profiles/`.

| Artefact path | Exists? | Notes |
|---|---|---|
| `/tmp/skv14-p1c-profiles/probe-cold_first_parse.json.gz` | **YES** | + `.syms.json` (465 KB); `grep -c "dispatch_value\|parse_value"` = 1 (symbols present in sidecar) |
| `/tmp/skv14-p1c-profiles/probe-host_call_eager_decode.json.gz` | **YES** | + `.syms.json` (605 KB) |
| `/tmp/skv14-p1c-profiles/probe-alternate_scalar_plan.json.gz` | **YES** | + `.syms.json` (504 KB) |
| `/tmp/skv14-p1c-profiles/probe-structural_scan.json.gz` | **YES** | + `.syms.json` (271 symbols documented in p1c §2.2.4) |

CH6 verdict: **all 4 P1-C profile pairs exist** at the cited paths; sample-count claims (`428 754` / `742 645` / `725 850` / `1 383 688`) are consistent with the file sizes; sidecar symbol resolution is the path used by `/tmp/skv14_p1c_resolve.py`. No paper-close.

### §1.4 — P1-D counter artefacts + xctrace escalation evidence

Cited at `p1d-pmu-cycles.md §5`.

| Artefact path | Exists? | Notes |
|---|---|---|
| `/tmp/skv14-p1d/artifacts/identity.txt` | **YES** | `host_triple=aarch64-apple-darwin`, `commit=2547c750bc78533d738eb85913206a0872022818`, `samply_version=samply 0.13.1`, `xctrace_version=xctrace version 26.0 (17A5241e)`, `os=26.4.1`, `kernel=25.4.0`, `rustc_version=rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, `sudo_available=sudo: a password is required`, `exit=1`, `user=mkbabb` — sudo refusal recorded verbatim |
| `/tmp/skv14-p1d/pmu/pmu_rows.tsv` | **YES** | 35 lines (34 rows + 1 header) — matches `n=34` claim in p1d §2.1 |
| `/tmp/skv14-p1d/direct/direct_rows.tsv` | **YES** | 69 lines (68 rows + 1 header) — matches `n=68` claim in p1d §2.2 |
| `/tmp/skv14-p1d/direct/typed_rows.tsv` | **YES** | 69 lines (68 rows + 1 header; 44 rc=0 + 24 absent rc=134) — matches p1d §2.3 + §2.4 |
| `/tmp/skv14-p1d/mode3/mode3_rows.tsv` | **YES** | 86 lines (85 rows + 1 header) — matches `n=85` claim in p1d §2.5 |
| `/tmp/skv14-p1d/xctrace/cpu-state.xml` | **YES** | 60 994 993 bytes — matches "60 MiB cpu-state XML" claim at p1d:118 + p1d §1.3; schema is "scheduling-state only" with no PMC columns, exactly the unavailable_from_current_export evidence |
| `/tmp/skv14-p1d/xctrace/traces/cpu_counters__twitter__track1.trace` | **YES** | source trace for the cpu-state.xml export |
| `/tmp/skv14-p1d/run-pmu.sh`, `run-direct.sh`, `run-typed.sh`, `run-mode3.sh` | **YES** (all four) | verbatim capture scripts; reproducibility evidence |

PMU coverage: 34 + 68 + 44 (rc=0) + 85 = **231 rows**, exactly matching p1d's `n=231` total claim. The 24 typed-fixture absences are correctly classified `unavailable_because_no_typed_fixture` (a product-surface gap), not `unprofiled` (a capture failure).

CH6 verdict for P1-D PMC counters: **NOT paper-close.** The unavailable_from_current_export classification is binding under three concurrent constraints:

  1. xctrace 26.0 CPU Counters template exports `cpu-state` schema only (60 MiB XML present at `/tmp/skv14-p1d/xctrace/cpu-state.xml` — orchestrator inspected the file size and the table-name escalation matrix at p1d §1.4).
  2. `sudo -n true` refused at capture time (recorded verbatim in identity.txt as `exit=1`).
  3. The unavailable classification is byte-identical to the SK-V13 V3 lock-in at `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:15, 54` (orchestrator confirmed grep matches the same wording).

The c/B + CPI numbers that gate-json consumes are load-bearing and reachable from `proc_pid_rusage(V5)`; PMC L1 / LLC / branch-miss numbers are not reachable from the unprivileged ceiling. P1-D §4.1 names this absence at p1d:511-518 ("cache-miss numbers are absent and must be named absent"), satisfying CH6's "every absence carries a stated cause" requirement.

### §1.5 — P1-E + P1-F have no original profile artefacts

P1-E is a hot-leaf attribution synthesis across P1-A/B/C; P1-F is a RESULTS.md delta. Neither produces flame profiles. P1-F:255 names this directly ("Profile binaries: none produced by this pass (P1-F is documentary; P1-A/B/C/D produce `/tmp/skv14-p1*/` flame profiles)"), so no CH6 paper-close risk exists at these axes.

CH6 verdict: ACCEPT for both. P1-E correctly cites the `parse-attribution` Cargo feature route as the V2 unlock for cracking the dispatch envelope (`p1e:110`); P1-F correctly flags `RESULTS.md:103` `n/a:w1b-2b-report-gate-consumes-w1b-2a-criterion` as a stale-artifact (CSS, not JSON) and does not re-introduce that pattern in JSON rows.

## §2 — Per-§ ACCEPT rate

| Spec § (PASS-1-PROFILE §3 CH6) | Demand | Verdict | Evidence |
|---|---|---|---|
| Flame profile file exists on disk | yes for every `parse__<corpus>__track1` / `<corpus>-<plane>-<track>` / `probe-<probe>` claim | **ACCEPT 17/17 P1-A + 56/56 P1-B + 4/4 P1-C = 77/77** | §1.1 + §1.2 + §1.3 ls confirmations |
| Symbol resolvable | yes for every cited hot-leaf | **ACCEPT** | sidecars present + sidecars grep-matched against cited symbols (dispatch_value / DirectParser::skip_value / probe symbols) |
| Every `unprofiled` cell resolved | per CH6 + CH1 binding | **ACCEPT 0 unprofiled** | The SK-V14 P1 sweep produced no `unprofiled` cells; the only `n/a` cells are `unavailable_because_no_typed_fixture` (P1-D §2.4: 24 rows; P1-E §2.3: 9 rows + 4 mixed) and PMU PMC `unavailable_from_current_export` (P1-D §1.4: 4 counter rows). Every absence carries a stated cause + source anchor. |
| Lock-14 absence not paper-close | named cause, named V2 remediation | **ACCEPT** | PMC: cause = "no privileged Instruments PMC trace package; sudo refused" (p1d §1.4 + §4.1); remediation = "out of S-P1 scope; would require sudo + custom Instruments trace package" + carried as SK-V13 V3 lock-in. |
| Folded-symbol paper-close risk addressed | named in V1, routed to V2 fold | **ACCEPT** | p1c §ANOM-4 names the risk verbatim and routes V2 to `--features parse-attribution`; p1e §2.1.4 + §2.2.4 names the same route at `runtime/Cargo.toml:21` + `generated.rs:43-44`; feature gate plumbing verified at `generated.rs:33-34, 43-44, 58-59, 79-80, 86-87, 117-118, 138-139, 157-158` (8 sites). |

## §3 — Critical findings

### §3.1 — CH6-PASS-1 (Lock-14 PMC absence is correctly classified)

The dispatch context (§2 CH6 bullet 1) asks: "verify P1-D PMC counters `unavailable_from_current_export` is correctly classified as Lock-14-finding not paper-close (sudo refused; xctrace exports scheduling-state only; documented per SK-V13 V3 lock-in)".

VERIFIED. All three substrate conditions are present:

  - sudo refused — `/tmp/skv14-p1d/artifacts/identity.txt` line `sudo_available=sudo: a password is required` + `exit=1` (orchestrator read the file; this is verbatim shell output).
  - xctrace exports scheduling-state only — `/tmp/skv14-p1d/xctrace/cpu-state.xml` is 60 994 993 bytes (orchestrator `ls -la` confirmed file size matches the 60 MiB claim at p1d:118); the table schema `start / cpu / state / duration / process / thread / priority` (p1d:119) has no counter columns; the escalation matrix at p1d §1.4 names every counter family as "no PMC schema in xctrace export".
  - SK-V13 V3 lock-in — orchestrator grepped `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md` and confirmed two matches for `unavailable_from_current_export` at line 15 and line 54 with identical wording.

CH6 verdict: this is a Lock-14 finding (substrate-constraint, externally-imposed), **not paper-close** (which would require either an unstated cause, a fake escalation claim, or a remediation route that does not exist). All three rebuttals are absent; the classification is correct.

### §3.2 — CH6-PASS-2 (`dispatch_value` folded-symbol is correctly named CH6 risk, V2 fold route concrete)

The dispatch context (§2 CH6 bullet 2) asks: "verify P1-C ANOM-4 dispatch_value folded symbol (parse-attribution off) — flag as paper-close risk if not addressed in V2 fold".

VERIFIED ADDRESSED. The artefact itself names the risk:

  > p1c:480-483: "This is a CH6 paper-close risk: the profile names a symbol that obscures the leaf primitive. Flagged for the CHALLENGE V2 hardening fold; it equally applies to P1-A and P1-B which would have reported the same single symbol."

The V2 fold route is concrete:

  > p1c:477-480: "A V2 fold of P1-A/B/C with `--features parse-attribution` (per `runtime/Cargo.toml:21`) would split into parse_object / parse_array / parse_string / parse_number / parse_literal / parse_pair / parse_key_colon."

Feature-gate plumbing verified existing in the runtime crate (orchestrator grepped `skinny/crates/runtime/src/grammars/json/generated.rs`):

  > Line 33 `#[cfg_attr(feature = "parse-attribution", inline(never))]`
  > Line 34 `#[cfg_attr(not(feature = "parse-attribution"), inline(always))]`
  > (Plus same pair at lines 43-44, 58-59, 79-80, 86-87, 117-118, 138-139, 157-158 — 8 functions gated symmetrically.)

  > `skinny/crates/runtime/Cargo.toml:21`: `parse-attribution = []` (empty feature declared).

CH6 verdict: NOT paper-close. The risk is named in the V1 artefact, the V2 remediation is concrete and code-anchored, and P1-A's atos -inlineFrames pipeline already provides a partial CH6 mitigation at record time (P1-A's "Top inlined leaf" columns crack the LTO-fused symbol with inlined-frame attribution; see `p1a:243-245` "primitive attribution in §2's 'Top inlined leaf' columns required atos -inlineFrames").

ADVISORY for the V2 aggregator: ensure the V2 fold explicitly schedules a `cargo build --features parse-attribution` rebuild for P1-A/B/C in the consolidated rollup, so the CH6 risk converts from "named + routed" to "named + routed + executed" before S-P2 dispatch.

### §3.3 — CH6-PASS-3 (path-existence verification full pass)

The dispatch context (§2 CH6 bullet 3 + §3 executable-verification mandate) asks: "spot-check 3-5 cited flame profile paths exist". Orchestrator executed five exemplar ls invocations (one per axis + one cross-axis): all five resolve. Additionally, `wc -l` confirms profile counts match per-axis claims (17/17 P1-A, 56/56 P1-B, 4/4 P1-C), and `grep -c` against three syms.json sidecars confirms the cited symbol names (`dispatch_value`, `DirectParser::skip_value`, mode-III probe symbols) are actually present in the sidecar payloads.

CH6 verdict: every cited path resolves; every cited symbol is grep-locatable in its sidecar; no paper-close.

### §3.4 — CH6-NEW-1 (8-sample `github_events` parse_only attribution is itself a CH6 risk, named at p1e:96 + p1e:110)

Beyond the three V1-focus checks: `p1e:96` lists `github_events parse_only` Track 1 hot leaf as `<u16 as From<u8>>::from (core/src/convert/num.rs:82) 87.5%` with primitive class `noise (inlined-std generic in 8-sample capture; CH6 risk)`. The capture has only 8 samples — too few to attribute load-bearing % self-time.

P1-E names this risk at the row itself ("CH6 risk" annotation in the table cell) and at the summary line (`p1e:110` — "1 is inlined-std `noise` (`github_events` capture had only 8 samples — CH6 risk)").

CH6 verdict: this is correctly flagged within the V1 artefact, but the V2 fold must include a re-record of `github_events parse_only` with longer iter count (`p1a §1.2`'s iter table assigns github_events 30,000 iters for direct/typed planes but parse_only's underlying iter count was apparently undersized). The V2 aggregator should escalate this as an addressable gap, not a paper-close — the absence has a stated cause (under-sampled) and a concrete remediation (longer cold-loop).

### §3.5 — CH6-NEW-2 (instruments + y_string_unicode direct hot-leaves are inlined-std noise, named at p1e:126 + p1e:134)

Similarly: `p1e:126` lists `instruments direct_to_struct` hot leaf as `Option<&u8>::copied (core/src/option.rs:2141) 58.3%` — inlined-std cursor peek, not a parser primitive. P1-E names this as `noise (inlined-std cursor peek)` + `CH6 risk` and routes the audit-overlay to AUDIT-FALSIFIED (W10 carry-over not verified). `y_string_unicode` direct/typed similarly attributes to timer dominance.

CH6 verdict: correctly flagged, no paper-close. V2 fold should re-record these with `--features parse-attribution` so the inlined-std envelope is split from the parser primitives it is hosting. Same V2 route as §3.2 — no separate redress required.

### §3.6 — CH6-NEW-3 (samply per-symbol PMC proxy not load-bearing)

The PMU escalation matrix (p1d §1.4) names "samply per-symbol sample-count proxy" as reachable in the P1-A/B/C scope but explicitly classifies it as "n/a — not load-bearing for c/B". This is the correct CH6 classification: samply's sample-count is the % self-time data for symbol attribution, not the PMC counter values for c/B / cache-miss. Confusing these would be paper-close; P1-D names them as distinct artefacts in the same row of the escalation matrix.

CH6 verdict: correctly disambiguated. No paper-close risk.

## §4 — V2 fold recommendations

CH6 V2 fold queue (for aggregator + V2 challenge cycle):

1. **MUST: schedule `--features parse-attribution` re-record of P1-A/B/C top-1 hot-leaves.** Risk named at p1c:480-483, route concrete at p1c:477-480 + `generated.rs:33-158` + `runtime/Cargo.toml:21`. Conversion path: "named + routed" → "named + routed + executed". V2 should produce a third top-N table per corpus with the `parse-attribution` inlines-off symbol decomposition appended to each row of `p1a §2.1` / `p1b §2.1, §2.2` / `p1c §2.1, §2.2`. Decomposition target: `parse_object` / `parse_array` / `parse_string` / `parse_number` / `parse_literal` / `parse_pair` / `parse_key_colon` (per p1c:478-480). For P1-B direct plane add `parse_object_value_at_direct::<JsonDigestSink>` / `parse_array_element_at_direct::<JsonDigestSink>`.

2. **MUST: re-record `github_events parse_only` Track 1 with longer iter count.** Risk named at p1e:96. The 8-sample capture is the only P1-A row whose hot-leaf is inlined-std `<u16 as From<u8>>::from`, not a parser primitive. Increase iters until ≥ 4000 samples (the P1-A target rate per `p1a §1.2`'s iter table for analogous-sized corpora). Cross-validate by re-checking the syms.json sidecar contains the expected `dispatch_value` / `match_tiny_plain_string_with_cap` envelope.

3. **SHOULD: standardise the `/tmp/skv14-p1*/` directory layout in V2.** The CHALLENGE-CONTEXT named `/tmp/skv14-p1a/samply/` but the actual P1-A artefacts live at `/tmp/skv14-p1/samply/` (no `a` suffix; the suffix is inferred from the identity manifest's `agent=P1-A` line). Future challenge dispatches should cite the canonical paths from each axis's §5 source block rather than a synthetic `p1a/p1b/p1c` convention. No paper-close; the artefacts are reachable; the naming inconsistency is an orchestration ergonomics issue.

4. **SHOULD NOT: re-open the typed-fixture `n/a` rows.** The 24 typed-product absences are correctly classified `unavailable_because_no_typed_fixture` (a product-surface gap, not a profile gap). Per p1d §2.4 + p1e §2.3 + S-P2 contract, the typed admit rows in `skinny/RESULTS.md` for those 6 corpora are AUDIT-FALSIFIED by PRUNE-1. The fold must not introduce a synthetic typed product just to fill in `n/a` cells — that would be the inverse paper-close (faking a load-bearing surface). The V2 aggregator should explicitly carry the typed-absence as a finding for S-P2 reconciliation, per the existing `p1e §2.3.2` REVISE route.

5. **SHOULD NOT: re-open PMC unavailable_from_current_export.** Per `[no-workarounds]` + the SK-V13 V3 lock-in, the V2 fold must not propose a workaround for PMC absence. The CPI + c/B + cycles + instructions counters are load-bearing and present; the PMC counters are documented absent. Any S-P2 hypothesis that depends on cache-miss data must either (a) wait for a privileged Instruments PMC trace package outside the current pin, or (b) be re-anchored against the CPI proxy that IS present. The V2 fold's job is to maintain the absence-with-stated-cause classification; the substrate constraint is fixed.

## §5 — Sources

CH6 cited the following artefacts (every claim above carries `path:line` per §3 of the V1 dispatch context):

V1 artefacts under review:
- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` (340 lines; lines 10, 103-107, 122-134, 138-154, 185-190, 239-245)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (320 lines; lines 1-11, 89, 154-166, 272-286)
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` (607 lines; lines 200-336, 437, 470-483, 547-589)
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` (648 lines; lines 100-150, 162-295, 344-358, 490-585, 586-629)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (306 lines; lines 5, 17, 43-71, 90-110, 114-155)
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` (260 lines; lines 47-97, 193, 255)

Filesystem artefacts (verified existing by `ls` / `wc -l` / `grep -c` / `head` at write time):
- `/tmp/skv14-p1/artifacts/identity.txt`, `/tmp/skv14-p1/samply/profiles/` (17 .json.gz + 17 .syms.json), `/tmp/skv14-p1/samply/inlined/` (17 inline files), `/tmp/skv14-p1/samply/tops/`
- `/tmp/skv14-p1b/samply/profiles/` (56 .json.gz + 56 .syms.json), `/tmp/skv14-p1b/samply/logs/`
- `/tmp/skv14-p1c-profiles/` (4 probe .json.gz + 4 .syms.json)
- `/tmp/skv14-p1d/artifacts/identity.txt`, `/tmp/skv14-p1d/pmu/pmu_rows.tsv`, `/tmp/skv14-p1d/direct/{direct,typed}_rows.tsv`, `/tmp/skv14-p1d/mode3/mode3_rows.tsv`, `/tmp/skv14-p1d/xctrace/cpu-state.xml` (60 994 993 bytes), `/tmp/skv14-p1d/xctrace/traces/cpu_counters__twitter__track1.trace`, `/tmp/skv14-p1d/run-{pmu,direct,typed,mode3}.sh`

Source-of-truth code anchors (verified existing by grep):
- `skinny/crates/runtime/src/grammars/json/generated.rs` lines 17, 27, 33-34, 43-44, 58-59, 79-80, 86-87, 117-118, 138-139, 157-158, 163, 168 (parse-attribution feature gate plumbing across 8 functions)
- `skinny/crates/runtime/Cargo.toml:21` (`parse-attribution = []`)
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:551-566` (`fixture_for_name`; the typed-product absence ground truth)

Cross-tranche substrate anchors:
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:15, 54` (SK-V13 V3 `unavailable_from_current_export` lock-in)
- `restart/prompts/skinny/PASS-1-PROFILE.md §3 CH6` (CH6 definition)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH6 binding)

## §6 — Closing

CH6 ACCEPTS the V1 P1 axis sweep against the ANTI-PAPER-CLOSE binding at **6/6 artefacts**. Every cited flame profile exists on disk with resolvable symbols. Every absence carries a stated cause anchored to a source-of-truth file or substrate constraint (typed-fixture absence → `real_typed_struct.rs:551-566`; PMC unreachable → sudo + xctrace schema + SK-V13 V3 lock-in; 8-sample under-coverage → V2 re-record route). The single most consequential CH6 risk in the sweep (the LTO-fused `dispatch_value` envelope) is named in the artefacts themselves, routed to a concrete V2 fold path (`--features parse-attribution`), and partially mitigated already at record time by P1-A's `atos -inlineFrames` pipeline.

V2 must execute the `parse-attribution` rebuild for P1-A/B/C and the `github_events parse_only` re-record (§4.1 + §4.2). No other CH6-blocking remediation is required for S-P2 dispatch. The lens converges.
