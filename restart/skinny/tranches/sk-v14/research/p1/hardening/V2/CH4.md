# S-P1 CHALLENGE V2 — CH4 COST (Profile Reproducibility, Confirming Pass)

Lens: CH4 — every §1 method block must carry verbatim commands a third
party can re-run; run id, host triple, build flags, samply version,
PMU access matrix all reproducible. Per `PASS-1-PROFILE.md §3 CH4`:
absent any one of those fields = CH4 FAIL for that artefact. V2 cycle
inherits the V1 baseline (93.5 % sub-axis ACCEPT) and tests the
F-V2-METHODOLOGY-1 light fold against the four V2 artefacts as
committed under `069ba203c`.

Pass: S-P1 Profile. Cycle: V2 (confirming). Date: 2026-05-23.
Author: CH4 lens agent (write-only). HARD CAP 30 min. No git mutation.
Authoritative dispatch: `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` §0-§5 + V2 micro-redispatch packet (commit `069ba203c`).

## §1 — V2 confirming-pass disposition

CF-1 (CH4-COHESION, RUSTFLAGS cross-artefact regime drift) was the
single orphan REVISE carried out of V1. V2 fold F-V2-METHODOLOGY-1
(Option A LIGHT — per-artefact `build_flags_regime` row disclosure,
not regime unification) landed across all four P1 frontmatters. This
pass verifies (a) the row is present in every frontmatter; (b) the
cohort assignment is correctly stated; (c) the cross-regime delta
refusal rule is documented at the P1-B §3 guard and cited verbatim
into the aggregator-consumer surface; (d) the V1 P1-A misstatement
(`native target CPU per Cargo.toml`) is corrected with an
explicit on-disk Cargo.toml cross-check.

| V2 artefact | `build_flags_regime` row present | Cohort | Aggregator-refusal cite | Cargo.toml cross-check | Verdict |
|---|---|---|---|---|---|
| P1-A `p1a-samply-mode-1.md` | YES (`:10`) | **{RUSTFLAGS-unset}** (corrected from V1 "native") | YES (`:10` last sentence: "V2 aggregator must refuse any cross-artefact Mbps/c/B delta where the `build_flags_regime` row mismatches") | YES (`:10` cites `skinny/Cargo.toml:78-86` — block does NOT carry `target-cpu`) | ACCEPT |
| P1-B `p1b-samply-mode-2.md` | YES (`:10`) | **{RUSTFLAGS-unset}** | YES (`:10` mid-sentence + `:185` §3 explicit "Build-flags regime guard" paragraph) | YES (`:10` cross-references `skinny/Cargo.toml [profile.release]` does not pin target-cpu) | ACCEPT |
| P1-C `p1c-samply-mode-3.md` | YES (`:17-25`) | **{RUSTFLAGS="-C target-cpu=native"}** | YES (`:23-25` "consumer-side aggregators are required to refuse a cross-row delta where `build_flags_regime` does not match") | n/a (frontmatter already pinned the override at build time per `:15-16`) | ACCEPT |
| P1-D `p1d-pmu-cycles.md` | YES (`:21-31`) | **{RUSTFLAGS="-C target-cpu=native"}** | YES (`:21-31` ties regime to P1-C + flags divergence from P1-B per `hardening/V1/CH4.md §3 CF-1`) | YES (`:21-23` notes workspace `[profile.release]` does not propagate `target-cpu`; pinned environment override) | ACCEPT |

**Per-§ ACCEPT rate at V2: 4/4 artefacts (100 %) on CF-1 closure;
all 31 V1 sub-axes recompute ACCEPT this cycle (P1-A `Build flags`
sub-axis lifts from REVISE→ACCEPT under F-V2-METHODOLOGY-1; P1-B
`Build flags` sub-axis lifts identically). Aggregate sub-axis
ACCEPT rate at V2: 31/31 = 100 %.** Hard cap status: 30 min budget;
this write ≈ 17 min wall.

## §2 — V2 per-artefact CF-1 closure evidence

Every cell carries a `path:line` cite, recomputed against the on-disk
V2 artefacts per `CHALLENGE-CONTEXT §3` executable-verification
mandate. `git rev-parse HEAD` this turn → `069ba203c413d46e7a5d465a128a983254e53841`;
`git log --oneline -1 069ba203c` → `docs(sk-v14-p1-profile): V2 light
micro-redispatch — five orphan REVISEs landed`; commit body lists
F-V2-METHODOLOGY-1 against P1-A/B/C/D as the four CH4-axis carriers.

### §2.1 — P1-A `p1a-samply-mode-1.md`

| CH4 sub-axis | Cite | V2 evidence | Verdict |
|---|---|---|---|
| `build_flags_regime` row presence | `p1a-samply-mode-1.md:10` | Single row labelled `build_flags_regime:` carries the cohort assignment + corrects the V1 "per Cargo.toml" misstatement. | ACCEPT |
| Cohort assignment | `p1a-samply-mode-1.md:10` (last sentence) | "this artefact and P1-B comprise the `RUSTFLAGS-unset` cohort". This is a **corrected cohort** vs V1 which (erroneously) implied P1-A sat in the `target-cpu=native` cohort by virtue of "per Cargo.toml". The correction is anchored on an explicit Cargo.toml citation (`skinny/Cargo.toml:78-86` — block does NOT carry `target-cpu`). | ACCEPT |
| Cargo.toml cross-check | `p1a-samply-mode-1.md:10` (mid-sentence) | "The `[profile.release]` block in `skinny/Cargo.toml:78-86` (`opt-level=3`, `lto="fat"`, `codegen-units=1`, `panic="abort"`, `debug=true`, `strip=false`, `split-debuginfo="packed"`) does **NOT** carry `target-cpu` — the Cargo manifest cannot set `RUSTFLAGS`; `target-cpu` is an environment-level override that this capture did not apply." Direct evidence supplied for the correction. | ACCEPT |
| Aggregator refusal cite | `p1a-samply-mode-1.md:10` (closing clause) | "V2 aggregator must refuse any cross-artefact Mbps/c/B delta where the `build_flags_regime` row mismatches" — the same refusal language is mirrored at P1-B `:185`, P1-C `:23-25`, P1-D `:21-31`. | ACCEPT |

### §2.2 — P1-B `p1b-samply-mode-2.md`

| CH4 sub-axis | Cite | V2 evidence | Verdict |
|---|---|---|---|
| `build_flags_regime` row presence | `p1b-samply-mode-2.md:10` | Single row labelled `build_flags_regime:` carries cohort assignment **`RUSTFLAGS unset`** in bold; explicit reference to `hardening/V1/CH4.md §3 CF-1`. | ACCEPT |
| Cohort assignment | `p1b-samply-mode-2.md:10` | "no `-C target-cpu=native` override either at the cargo invocation level or via `skinny/Cargo.toml [profile.release]`, which does not pin target-cpu". P1-B is the load-bearing reference artefact for the unset cohort (cleanest disclosure of the four per `hardening/V1/CH4.md §2.2`). | ACCEPT |
| §3 cross-regime delta refusal guard | `p1b-samply-mode-2.md:185` | New §3 paragraph "Build-flags regime guard (cross-artefact comparator rule)" — explicitly: "**No P1-C / P1-D cross-artefact comparison is computed in §3 because the build-flag regime differs** (P1-C / P1-D both pin `RUSTFLAGS="-C target-cpu=native"`, per `hardening/V1/CH4.md §2.3 + §2.4`)". Closes the V1 CF-1 cross-row arithmetic risk in the §3 results table itself. | ACCEPT |
| Canonical refusal target documented | `p1b-samply-mode-2.md:185` | The `twitter` Track 1 direct example (P1-B 11037 Mbps / 3.00 c/B vs P1-D 11627 Mbps / 2.938 c/B; 5.3 % Mbps + 2.1 % c/B drift) is named as the canonical refusal target. This matches verbatim the V1 CF-1 §3 illustration in `hardening/V1/CH4.md §3 CF-1` (lines 111-116). | ACCEPT |
| Cross-citation symmetry | `p1b-samply-mode-2.md:10`, `:185` | Both citation points refer to `hardening/V1/CH4.md §3 CF-1` as the binding source-of-truth for the refusal rule — the artefact-side guard is sourced from the lens-side finding, closing the loop. | ACCEPT |

### §2.3 — P1-C `p1c-samply-mode-3.md`

| CH4 sub-axis | Cite | V2 evidence | Verdict |
|---|---|---|---|
| `build_flags_regime` row presence | `p1c-samply-mode-3.md:17-25` | 9-line frontmatter block labelled `build_flags_regime:` carries cohort assignment **`RUSTFLAGS="-C target-cpu=native"`** + cross-cohort delineation. | ACCEPT |
| Cohort assignment | `p1c-samply-mode-3.md:19-21` | "P1-C/D share this regime; P1-A/B do not (P1-A: RUSTFLAGS not set explicitly, native-CPU NOT pinned because Cargo.toml does not propagate target-cpu; P1-B: explicit `RUSTFLAGS unset` disclosure)". This **encodes the corrected V2 cohort assignment** matching the P1-A correction at `p1a-samply-mode-1.md:10`. | ACCEPT |
| Build-block backed by frontmatter | `p1c-samply-mode-3.md:17-18` | The row references `§1.1` block (`RUSTFLAGS="-C target-cpu=native" cargo build`) — the frontmatter declaration is anchored to an actual verbatim shell command (cited at `:37` per V1 §2.3). | ACCEPT |
| Aggregator refusal cite | `p1c-samply-mode-3.md:23-25` | "consumer-side aggregators are required to refuse a cross-row delta where `build_flags_regime` does not match (per CH4 F-V2-METHODOLOGY-1 Option A binding)". Names the fold packet by ID. | ACCEPT |
| Illustrative refusal target | `p1c-samply-mode-3.md:22-23` | The `twitter` 11037 Mbps vs 11627 Mbps (5.3 % delta) example is named, symmetric with P1-B `:185`. | ACCEPT |

### §2.4 — P1-D `p1d-pmu-cycles.md`

| CH4 sub-axis | Cite | V2 evidence | Verdict |
|---|---|---|---|
| `build_flags_regime` row presence | `p1d-pmu-cycles.md:21-31` | 11-line frontmatter block labelled `build_flags_regime:` carries cohort assignment + both-build-invocation re-verification. | ACCEPT |
| Cohort assignment | `p1d-pmu-cycles.md:28-30` | "same regime as P1-C (`p1c-samply-mode-3.md:37, :606`); diverges from P1-B (`RUSTFLAGS unset`, `p1b-samply-mode-2.md:311`) per CH4 CF-1 cross-artefact drift". Cohort assignment matches the corrected V2 picture. | ACCEPT |
| Two-target-dir regime re-confirmation | `p1d-pmu-cycles.md:23-27` | "Both `cargo build` invocations in §1.1 (`/tmp/skv14-p1d-target` parse+direct+typed at line 41 and `/tmp/skv14-p1d-mode3-target` mode-III scratch crate at line 62) carry the same `RUSTFLAGS="-C target-cpu=native"` prefix verbatim — no build flag divergence across the two target dirs." This addresses the P1-D-specific risk that the mode-III scratch crate (built under a separate target dir) might silently diverge from the main target dir's regime. | ACCEPT |
| Re-confirmation freshness | `p1d-pmu-cycles.md:31` | "re-confirmed verbatim against §1.1 lines 41 + 62 this turn" — the V2 cycle re-validated the build-block lines, not just the V1 disclosure. | ACCEPT |

## §3 — Cohort assignment + cross-regime refusal lattice

The CF-1 closure rests on three load-bearing assertions, each
verified on-disk this turn:

### §3.1 — Cohort lattice (corrected at V2)

| Cohort | Artefacts | Frontmatter cites | Build-block cites |
|---|---|---|---|
| **RUSTFLAGS-unset** (default aarch64 baseline) | P1-A, P1-B | `p1a-samply-mode-1.md:10`, `p1b-samply-mode-2.md:10` | P1-A: no `RUSTFLAGS=` prefix at `:33-36`; P1-B: no `RUSTFLAGS=` prefix at `:21-22` |
| **RUSTFLAGS="-C target-cpu=native"** (Apple-Silicon-specialised codegen) | P1-C, P1-D | `p1c-samply-mode-3.md:17-25`, `p1d-pmu-cycles.md:21-31` | P1-C: `RUSTFLAGS="-C target-cpu=native" cargo build ...` at `:37` (V1 §2.3 cite); P1-D: same prefix at `:41` + `:62` (mode-III scratch crate target dir) |

Cohort assignment is **canonical at V2** — every artefact's
frontmatter explicitly names its cohort + names the cohort of the
other three artefacts; no implicit cohort assignment remains. The V1
P1-A misstatement is corrected at `p1a-samply-mode-1.md:10` with the
text "**CORRECTED here**" + the `skinny/Cargo.toml:78-86` evidence.

### §3.2 — Cross-regime delta refusal rule

The refusal rule is documented at four citation points across the
four artefacts, with matching semantic content (re-verified verbatim
this turn):

| Artefact | Cite | Refusal language | Match against canonical CF-1 statement |
|---|---|---|---|
| P1-A | `:10` | "V2 aggregator must refuse any cross-artefact Mbps/c/B delta where the `build_flags_regime` row mismatches" | matches |
| P1-B | `:10` | "no Mbps or c/B cell in this artefact is directly comparable to any P1-C / P1-D cell ... must be refused by the aggregator until both regimes are unified or the row carries an explicit per-row `RUSTFLAGS` qualifier" | matches; extends with "or per-row qualifier" |
| P1-B §3 guard | `:185` | "regime-mismatch; not directly comparable ... must be refused at the aggregator boundary" | matches; placed at the results-table boundary itself |
| P1-C | `:23-25` | "consumer-side aggregators are required to refuse a cross-row delta where `build_flags_regime` does not match (per CH4 F-V2-METHODOLOGY-1 Option A binding)" | matches; names the fold packet |
| P1-D | `:21-31` | "diverges from P1-B (`RUSTFLAGS unset`, `p1b-samply-mode-2.md:311`) per CH4 CF-1 cross-artefact drift" | matches; ties divergence to V1 CF-1 |

Every refusal-rule citation point names the same canonical refusal
target (`twitter` Track 1 direct: P1-B 11037 Mbps vs P1-D 11627 Mbps;
5.3 % Mbps + 2.1 % c/B drift) at one or more of P1-B `:10`, P1-B
`:185`, P1-C `:22-23`, `hardening/V1/CH4.md §3 CF-1` (lines 111-116).
This is a **canonically encoded refusal lattice** — a third party
attempting to subtract any P1-B cell from any P1-C/D cell can find
the refusal rule at any of four front-of-document points.

### §3.3 — Aggregator-binding completeness

The fold landed as Option A (LIGHT) per
`hardening/HARDENING-S-P1-V1-CONSOLIDATED.md §2.3 lines 280-296` —
i.e. disclosure-only, not regime unification. The heavy
F-V2-P1ABC-RERECORD packet (re-run P1-A/B under `target-cpu=native`,
or re-run P1-C/D under `RUSTFLAGS unset`) is explicitly deferred to
S-P2 design per the V1 aggregator's Option X (parse-attribution
rebuild is the primitive-design ground-truth surface, not a
lens-correctness fix). CH4 at V2 binds to Option A as the correct
disposition for the SK-V14 hardening cycle; Option B is correctly
out of scope this pass.

## §4 — V2 critical findings

### CF-V2-1 (P1-A cohort correction is a genuine factual revision; CH4-METHOD) — ACCEPT-with-note

The V1 P1-A frontmatter at `:9` asserted "native target CPU per
`skinny/Cargo.toml`". V2 P1-A at `:10` explicitly states "**CORRECTED
here**" with the Cargo.toml `:78-86` block cited as evidence (no
`target-cpu` line in `[profile.release]`). This is the only V2 fold
that retracts a V1 factual claim, not just disclosure-extends one.
The correction is internally consistent (P1-A is now in the
`RUSTFLAGS-unset` cohort) and externally consistent (P1-B at `:10`
already named the same cohort assignment for P1-A). **No additional
REVISE pressure**; the correction lands cleanly.

The cohort discovery in the V2 micro-redispatch packet ({P1-A, P1-B}
both unset; {P1-C, P1-D} both native) matches the V2 commit body of
`069ba203c` exactly:

> P1-A is actually RUSTFLAGS-unset cohort (same as P1-B).

This is the canonical V2 cohort statement and is faithfully encoded
across all four V2 frontmatters.

### CF-V2-2 (refusal rule is encoded but not enforced; CH4-AGGREGATOR-INTERFACE) — ACCEPT-with-note

The refusal rule is **documented** in four places (per §3.2) but
the aggregator-side enforcement remains conceptual — there is no
machine-readable schema check yet. `restart/skinny/tranches/sk-v14/SYNTHESIS.md §2`
telemetry schema does not yet name a `build_flags_regime` column
that would let a downstream comparator silently refuse a row pair.
**This is correctly outside the V2 hardening fold's scope** (V2
was disclosure-only) — but flagging as a V3-or-S-P2-input item so
that a future schema-pass binds the refusal to a mechanical check
rather than to prose. **No CH4 V2 REVISE**.

### CF-V2-3 (CF-2 + CF-3 + CF-4 carry forward intact; CH4-INHERIT) — ACCEPT

The three V1 CH4 findings that were ACCEPT-with-note (not REVISE)
all carry forward unchanged at V2 — the V2 fold packet did not
touch them and did not need to:

- **CF-2** (clock-overhead inflation on small corpora) — P1-B §4
  anomaly 6 (`p1b-samply-mode-2.md:280-282` recomputed this turn,
  text unchanged at V2) remains the canonical floor disclosure.
- **CF-3** (P1-C aggregate-only attribution) — P1-C §2.2
  (`p1c-samply-mode-3.md:189-194` recomputed this turn) remains
  the canonical grain disclosure; the `escalation §C` per-corpus
  capture remains a S-P2 item per V1 §4.3.
- **CF-4** (P1-D PMU matrix vs SK-V13 V3 parity) — P1-D §1.4 PMU
  matrix (`p1d-pmu-cycles.md:126-150` recomputed this turn)
  remains byte-identical to the SK-V13 V3 baseline; `identity.txt`
  `sudo_available=sudo: a password is required` still binding.

None of CF-2/3/4 lifts to REVISE at V2; none degrades. The V1
ACCEPT-with-note disposition is the correct V2 disposition for
each.

### CF-V2-4 (no new V2 REVISE introduced) — ACCEPT

The V2 confirming pass surfaces **zero new REVISE-class findings**.
Every V1 sub-axis that was ACCEPT remains ACCEPT; the two V1
REVISE sub-axes (P1-A `Build flags`, P1-B `Build flags`) lift to
ACCEPT under F-V2-METHODOLOGY-1. The V2 fold did not introduce
regression on any other CH4 sub-axis (the 31-row sub-axis matrix
is intact; the V1 verbatim-commands + host-triple + samply-version
+ run-id + PMU-access-matrix evidence rows all re-verify this turn
against the V2 artefacts).

## §5 — V3 fold recommendations (zero binding)

Per `ORCHESTRATOR.md §3Z` convergence rule, V2 ACCEPT-with-zero-REVISE
is sufficient to converge CH4 once a second cycle confirms (≥95 % × 2
cycles, zero orphan REVISEs). CH4's V1 was ACCEPT-with-REVISE (93.5 %);
CH4's V2 is ACCEPT-with-zero-REVISE (100 %). One more clean cycle
(V3) would lock convergence per the §3Z gate.

Non-binding observations for orchestrator routing:

1. **Schema-level refusal binding** (CF-V2-2 escalation) — add
   `build_flags_regime` as a named column in `SYNTHESIS.md §2`
   telemetry schema so a downstream aggregator can mechanically
   refuse cross-regime row arithmetic rather than relying on prose
   citation lattices. Pre-S-P2 hook; not a V3 CH4 blocker.

2. **F-V2-P1ABC-RERECORD remains the right S-P2 design item** —
   the heavy-fold "unify all four artefacts under one RUSTFLAGS
   regime" remains the architecturally correct way to close the
   regime drift permanently. Option A's per-row disclosure
   strategy is the right hardening-cycle answer (low risk, lands
   cleanly); Option B is the right S-P2 design answer (full
   regime unification + re-capture). Both are correctly scoped.

3. **No CH4 finding pre-blocks REDRESS** at V2 — CF-V2-1..4 do
   not re-propose any pre-blocked REDRESS route per
   `CHALLENGE-CONTEXT §2` CH3 cross-check. CF-V2-1 is a factual
   correction in a frontmatter; CF-V2-2 is a schema observation;
   CF-V2-3 is a clean carry-forward; CF-V2-4 is a no-finding
   row. All upstream of any REDRESS gate.

## §6 — V2 convergence vote

Per `PASS-1-PROFILE.md §3 CH4` + `ORCHESTRATOR.md §3Z`:

- **ACCEPT**: 4/4 artefacts pass the load-bearing CH4 axis at V2
  (verbatim commands, host triple, samply version, run id, PMU
  matrix, **plus** the new `build_flags_regime` row + cross-regime
  refusal cite + Cargo.toml cross-check evidence — all present and
  on-disk verified).
- **REVISE**: zero V2 orphan REVISEs. The V1 CF-1 REVISE is
  **closed**; the two V1 sub-axis REVISEs (P1-A `Build flags`,
  P1-B `Build flags`) lift to ACCEPT.
- **Per-§ ACCEPT rate at V2**: §2.1 P1-A 4/4 sub-axes ACCEPT
  (build-flags row + cohort + Cargo.toml cross-check + refusal
  cite, all new at V2); §2.2 P1-B 5/5 sub-axes ACCEPT (incl. §3
  guard + canonical refusal target named); §2.3 P1-C 5/5 sub-axes
  ACCEPT (incl. cohort delineation + Option A binding cite); §2.4
  P1-D 4/4 sub-axes ACCEPT (incl. two-target-dir re-confirmation +
  freshness note). The 31 V1 sub-axes all recompute ACCEPT this
  cycle (V1 CF-1's two REVISEs lift cleanly).
  **V2 aggregate sub-axis ACCEPT rate: 18 new V2 sub-axes (4+5+5+4)
  + 31 lifted V1 sub-axes = 49/49 = 100 %.** Counting only the V1
  base matrix lifted by F-V2-METHODOLOGY-1: 31/31 = 100 %.

CH4 V2 vote: **ACCEPT** (zero orphan REVISEs; ≥95 % gate cleared at
100 %). Per ORCHESTRATOR §3Z, this is the first of two clean cycles
required for CH4 lens convergence; V3 confirming pass would lock
convergence. CH4 carries zero blocker into the V2 aggregator.

## §7 — Sources (every cite re-verified this turn)

- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` §0-§5 (V2 dispatch authority; inherited from V1).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH4.md` (V1 baseline; CF-1..CF-4 + §4 fold recommendations 1-5; 93.5 % aggregate).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md` §2.3 (F-V2-METHODOLOGY-1 Option A vs Option B disposition; binding for V2 fold scope) + §3 expected V2 lift table (CH4 93.5 % → ≈100 %).
- `restart/prompts/skinny/PASS-1-PROFILE.md §3 CH4` (lens definition; binding).
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH4 def), §3Z (convergence rule).
- V2 artefacts (commit `069ba203c`, dated 2026-05-23):
  - `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` `:10` (build_flags_regime row + V1 correction + Cargo.toml `:78-86` cross-check + aggregator refusal cite).
  - `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` `:10` (build_flags_regime row + cohort + canonical refusal target) + `:185` (§3 Build-flags regime guard paragraph; canonical refusal target named at results-table boundary).
  - `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` `:17-25` (build_flags_regime row + cohort delineation + Option A binding cite).
  - `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` `:21-31` (build_flags_regime row + two-target-dir re-confirmation + freshness verification this turn).
- Host-side verification:
  - `git rev-parse HEAD` → `069ba203c413d46e7a5d465a128a983254e53841` (matches the V2 commit cited in dispatch).
  - `git log --oneline -1 069ba203c` → `docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan REVISEs landed` (matches expected commit subject).
  - `grep -c "build_flags_regime"` across all four V2 P1 frontmatters → P1-A 1, P1-B 2, P1-C 2, P1-D 2 (every artefact carries the row at least once; P1-B/C/D carry it twice because the §3 guard / cross-cohort statement re-cites the canonical column name).
  - `wc -l` across four V2 P1 frontmatters → P1-A 343, P1-B 323, P1-C 616, P1-D 669 (matches V2 artefact set).
