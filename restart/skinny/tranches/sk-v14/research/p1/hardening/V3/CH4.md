# S-P1 CHALLENGE V3 — CH4 COST (Profile Reproducibility, Pure Confirming Pass)

Lens: CH4 — every §1 method block must carry verbatim commands a third
party can re-run; run id, host triple, build flags, samply version,
PMU access matrix all reproducible. Per `PASS-1-PROFILE.md §3 CH4`:
absent any one of those fields = CH4 FAIL for that artefact. V3 is the
pure confirming cycle over the V2-locked baseline; per
`ORCHESTRATOR.md §3Z`, two clean cycles (V2 + V3) at ≥95 % with zero
orphan REVISEs lock convergence.

Pass: S-P1 Profile. Cycle: V3 (confirming, no fold packets pending).
Date: 2026-05-23. Author: CH4 lens agent (write-only). HARD CAP 30 min.
No git mutation. Authoritative dispatch:
`restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md`
§0-§5 + V2 baseline at `hardening/V2/CH4.md` (100 %, CF-1 closed).

## §1 — V3 disposition summary

V2 CH4 reached **100 % sub-axis ACCEPT** (31/31 V1-base + 18 new V2
disclosure sub-axes = 49/49) with **zero orphan REVISEs**. The single
V1 orphan REVISE (CF-1: RUSTFLAGS cross-artefact regime drift) closed
via Option A LIGHT (per-artefact `build_flags_regime` row disclosure)
and the V1 P1-A cohort misstatement was factually corrected
("native target CPU per `skinny/Cargo.toml`" → "RUSTFLAGS-unset
cohort, matching P1-B"). V3 verifies that this V2 baseline holds:

(a) every V2 frontmatter `build_flags_regime` row remains on disk
    with identical line numbers;
(b) the cohort assignment encoded across the four artefacts remains
    internally consistent;
(c) the four-point refusal-rule lattice remains canonically encoded;
(d) the on-disk Cargo.toml `[profile.release]` cross-check still
    confirms `target-cpu` is NOT pinned in the manifest;
(e) no new REVISE-class finding surfaces on re-recompute of the
    V1 31-row sub-axis matrix or the V2 18-row disclosure matrix.

| V2 artefact | Frontmatter row at expected `:line` | Cohort encoded matches V2 | Refusal cite intact | Cargo.toml cross-check intact | V3 verdict |
|---|---|---|---|---|---|
| P1-A `p1a-samply-mode-1.md` | YES (`:10`) | YES (`{RUSTFLAGS-unset}`) | YES (`:10`) | YES (`skinny/Cargo.toml:78-86`, recomputed this turn) | ACCEPT |
| P1-B `p1b-samply-mode-2.md` | YES (`:10`) | YES (`{RUSTFLAGS unset}`) | YES (`:10`, `:185` §3 guard) | YES (`:10` references same Cargo.toml block) | ACCEPT |
| P1-C `p1c-samply-mode-3.md` | YES (`:17-25`) | YES (`{RUSTFLAGS="-C target-cpu=native"}`) | YES (`:23-25` names Option A binding) | n/a (build-block pins override; `:37`) | ACCEPT |
| P1-D `p1d-pmu-cycles.md` | YES (`:21-31`) | YES (`{RUSTFLAGS="-C target-cpu=native"}`) | YES (`:21-31` ties to P1-C + CF-1) | YES (`:21-23` notes workspace does not propagate `target-cpu`) | ACCEPT |

**V3 aggregate sub-axis ACCEPT rate: 49/49 = 100 %** (31 V1-base +
18 V2-disclosure, all recompute ACCEPT this cycle). Hard cap status:
30 min budget; this write ≈ 14 min wall.

## §2 — V3 per-artefact re-verification evidence

Every cell recomputed against on-disk V2 artefacts per
`CHALLENGE-CONTEXT §3` executable-verification mandate. Host-side
verification this turn:

- `git rev-parse HEAD` → `4ad8f1949099829b7ad723ddfd7eeb2a40cf61cd`
  (head moved one commit past V2 `069ba203c` — the V2 hardening
  consolidated landed; the four V2 P1 frontmatters were not touched
  by `4ad8f1949`).
- `git log --oneline -1 069ba203c` → `docs(sk-v14-p1-profile): V2
  light micro-redispatch — five orphan REVISEs landed` (V2 commit
  subject identical to V2 CH4 §2 cite).
- `wc -l` across four V2 P1 frontmatters → P1-A 343, P1-B 323,
  P1-C 616, P1-D 669. **Matches V2 CH4 §7 exactly.**
- `grep -c "build_flags_regime"` across four V2 P1 frontmatters →
  P1-A 1, P1-B 2, P1-C 2, P1-D 2. **Matches V2 CH4 §7 exactly.**

### §2.1 — P1-A `p1a-samply-mode-1.md` (V3 re-verify)

| CH4 sub-axis (V2 carrier) | Cite | V3 evidence | Verdict |
|---|---|---|---|
| `build_flags_regime` row | `:10` | Row present verbatim ("RUSTFLAGS NOT SET EXPLICITLY (default aarch64-apple-darwin baseline; native-CPU NOT pinned)"); V1 correction marker `**CORRECTED here**` present. | ACCEPT |
| Cohort assignment | `:10` (closing clause) | "this artefact and P1-B comprise the `RUSTFLAGS-unset` cohort" — text unchanged from V2 capture. | ACCEPT |
| Cargo.toml cross-check | `:10` (mid-sentence) | "`skinny/Cargo.toml:78-86` (`opt-level=3`, `lto="fat"`, `codegen-units=1`, `panic="abort"`, `debug=true`, `strip=false`, `split-debuginfo="packed"`) does **NOT** carry `target-cpu`" — recomputed against `skinny/Cargo.toml:78-86` this turn (block contents byte-identical to the V2 cite; `grep "target-cpu" skinny/Cargo.toml` returns no match). | ACCEPT |
| Aggregator refusal cite | `:10` (last sentence) | "V2 aggregator must refuse any cross-artefact Mbps/c/B delta where the `build_flags_regime` row mismatches" — verbatim, matches V2 §2.1. | ACCEPT |

### §2.2 — P1-B `p1b-samply-mode-2.md` (V3 re-verify)

| CH4 sub-axis (V2 carrier) | Cite | V3 evidence | Verdict |
|---|---|---|---|
| `build_flags_regime` row | `:10` | Row labelled `build_flags_regime: **RUSTFLAGS unset**` present; explicit `hardening/V1/CH4.md §3 CF-1` back-reference present. | ACCEPT |
| Cohort assignment | `:10` | "default aarch64 baseline; no `-C target-cpu=native` override either at the cargo invocation level or via `skinny/Cargo.toml [profile.release]`, which does not pin target-cpu" — unchanged. | ACCEPT |
| §3 cross-regime delta refusal guard | `:185` | "**Build-flags regime guard (cross-artefact comparator rule).**" paragraph present; "**No P1-C / P1-D cross-artefact comparison is computed in §3 because the build-flag regime differs**" verbatim. | ACCEPT |
| Canonical refusal target | `:185` | `twitter` Track 1 direct example (P1-B 11037 Mbps / 3.00 c/B vs P1-D 11627 Mbps / 2.938 c/B; 5.3 % / 2.1 % drift) present verbatim, cross-references `hardening/V1/CH4.md §3 CF-1`. | ACCEPT |
| Cross-citation symmetry | `:10`, `:185` | Both citation points reference `hardening/V1/CH4.md §3 CF-1` as binding refusal-rule source. | ACCEPT |

### §2.3 — P1-C `p1c-samply-mode-3.md` (V3 re-verify)

| CH4 sub-axis (V2 carrier) | Cite | V3 evidence | Verdict |
|---|---|---|---|
| `build_flags_regime` row | `:17-25` | 9-line frontmatter block carries cohort assignment + cross-cohort delineation; lines 17 ("build_flags_regime: `RUSTFLAGS="-C target-cpu=native"` explicitly pinned at"), 19-21 (P1-C/D share regime; P1-A/B do not), 23-25 (aggregator refusal binding) all verbatim. | ACCEPT |
| Cohort assignment | `:19-21` | "P1-C/D share this regime; P1-A/B do not (P1-A: RUSTFLAGS not set explicitly, native-CPU NOT pinned because Cargo.toml does not propagate target-cpu; P1-B: explicit `RUSTFLAGS unset` disclosure)" — matches the corrected V2 cohort encoding. | ACCEPT |
| Build-block anchored | `:17-18` | Row references `§1.1` block (`RUSTFLAGS="-C target-cpu=native" cargo build`). | ACCEPT |
| Aggregator refusal cite | `:23-25` | "consumer-side aggregators are required to refuse a cross-row delta where `build_flags_regime` does not match (per CH4 F-V2-METHODOLOGY-1 Option A binding)" — names fold packet by ID, unchanged. | ACCEPT |
| Illustrative refusal target | `:22-23` | `twitter` 11037 vs 11627 Mbps (5.3 % delta) named; symmetric with P1-B `:185`. | ACCEPT |

### §2.4 — P1-D `p1d-pmu-cycles.md` (V3 re-verify)

| CH4 sub-axis (V2 carrier) | Cite | V3 evidence | Verdict |
|---|---|---|---|
| `build_flags_regime` row | `:21-31` | 11-line frontmatter block present verbatim; carries cohort + both-build-invocation re-verification across two target dirs. | ACCEPT |
| Cohort assignment | `:28-30` | "same regime as P1-C (`p1c-samply-mode-3.md:37, :606`); diverges from P1-B (`RUSTFLAGS unset`, `p1b-samply-mode-2.md:311`) per CH4 CF-1 cross-artefact drift" — matches V2 cohort encoding. | ACCEPT |
| Two-target-dir regime re-confirmation | `:23-27` | "Both `cargo build` invocations in §1.1 (`/tmp/skv14-p1d-target` parse+direct+typed at line 41 and `/tmp/skv14-p1d-mode3-target` mode-III scratch crate at line 62) carry the same `RUSTFLAGS="-C target-cpu=native"` prefix verbatim — no build flag divergence across the two target dirs." | ACCEPT |
| Re-confirmation freshness | `:31` | "re-confirmed verbatim against §1.1 lines 41 + 62 this turn" — freshness language from V2 cycle preserved. | ACCEPT |

## §3 — V3 carry-forward of V2 ACCEPT-with-note findings

The four V2 ACCEPT-with-note findings (CF-V2-1..CF-V2-4) carry
forward intact at V3, none lifts to REVISE, none degrades:

| V2 finding | V2 disposition | V3 re-verify | V3 disposition |
|---|---|---|---|
| **CF-V2-1** (P1-A cohort correction is a genuine factual revision; CH4-METHOD) | ACCEPT-with-note | `**CORRECTED here**` marker still present at `p1a-samply-mode-1.md:10`; cohort assignment still internally + externally consistent (P1-B `:10` still names P1-A in unset cohort); no further factual retractions surfaced. | ACCEPT-with-note (unchanged) |
| **CF-V2-2** (refusal rule is encoded but not enforced; CH4-AGGREGATOR-INTERFACE) | ACCEPT-with-note | Refusal rule still documented at four citation points (§2 above); no `build_flags_regime` column added to `SYNTHESIS.md §2` telemetry schema this cycle (correctly out of V3 confirming-pass scope); finding remains a V3-or-S-P2-input observation only. | ACCEPT-with-note (unchanged) |
| **CF-V2-3** (CF-2/CF-3/CF-4 carry forward intact; CH4-INHERIT) | ACCEPT | All three V1 ACCEPT-with-note findings still hold: CF-2 clock-overhead floor (P1-B §4 anomaly 6 `:280-282`), CF-3 P1-C aggregate-only attribution (`p1c-samply-mode-3.md:189-194`), CF-4 PMU-matrix parity with SK-V13 V3 (`p1d-pmu-cycles.md:126-150` byte-identical to baseline). None touched between V2 and V3. | ACCEPT (unchanged) |
| **CF-V2-4** (no new V2 REVISE introduced) | ACCEPT | V3 confirming pass surfaces **zero new REVISE-class findings** as well; the 31 V1-base sub-axes + 18 V2-disclosure sub-axes all recompute ACCEPT. | ACCEPT (unchanged) |

## §4 — V3 critical findings (new this cycle)

### CF-V3-1 (V3 surfaces zero new findings — confirming pass is clean; CH4-CONVERGENCE) — ACCEPT

The V3 confirming pass introduces **no new REVISE**, **no new
ACCEPT-with-note**, **no factual disagreement** with V2. Every cite
re-verified against on-disk content this turn matches the V2 cite
verbatim; every line number cited in V2 (P1-A `:10`, P1-B `:10` +
`:185`, P1-C `:17-25`, P1-D `:21-31`) addresses the expected
content; every cross-reference (V1 CH4 CF-1, fold packet
F-V2-METHODOLOGY-1, Option A binding) resolves intact.

The four-point refusal-rule lattice continues to encode the same
canonical refusal target (`twitter` 11037 vs 11627 Mbps; 5.3 % /
2.1 % drift) at the same four citation points (P1-B `:10`, P1-B
`:185`, P1-C `:22-23`, `hardening/V1/CH4.md §3 CF-1` lines
111-116). The cohort lattice continues to encode the correct
2x2 assignment ({P1-A, P1-B} unset; {P1-C, P1-D} native).

### CF-V3-2 (convergence gate cleared at V3; CH4-CONVERGENCE) — ACCEPT

Per `ORCHESTRATOR.md §3Z`: convergence requires ≥95 % ACCEPT
across two consecutive cycles with zero orphan REVISEs.

- V2 cycle: 100 % (49/49 sub-axes ACCEPT, zero orphan REVISEs).
- V3 cycle: 100 % (49/49 sub-axes ACCEPT, zero orphan REVISEs).

**Two consecutive cycles at ≥95 % cleared.** Zero orphan REVISEs
sustained across both. CH4 convergence gate **CLEARED at V3** per
the §3Z rule.

## §5 — V4 fold recommendations (zero binding)

V3 is a pure confirming pass with zero new findings. No V4 fold
packets are emitted from this lens. The two non-binding
observations from V2 §5 carry forward for orchestrator routing
context, neither blocks any V3 or V4 cycle:

1. **Schema-level refusal binding** (V2 CF-V2-2 escalation) — adding
   `build_flags_regime` as a named column in `SYNTHESIS.md §2`
   telemetry schema remains a pre-S-P2 hook; not a CH4 blocker at
   any V cycle.

2. **F-V2-P1ABC-RERECORD remains the right S-P2 design item** —
   the heavy-fold regime unification continues to be architecturally
   correct for permanent CF-1 closure; Option A's per-row disclosure
   remains the right hardening-cycle answer; both correctly scoped
   and unchanged.

## §6 — V3 convergence vote

Per `PASS-1-PROFILE.md §3 CH4` + `ORCHESTRATOR.md §3Z`:

- **ACCEPT**: 4/4 V2 artefacts continue to pass the load-bearing
  CH4 axis (verbatim commands, host triple, samply version, run id,
  PMU matrix, `build_flags_regime` row, cross-regime refusal cite,
  Cargo.toml cross-check — all on-disk re-verified this turn).
- **REVISE**: zero V3 orphan REVISEs. V2 fold closure of CF-1
  remains intact; the two V1 sub-axis REVISEs (P1-A `Build flags`,
  P1-B `Build flags`) remain lifted to ACCEPT under
  F-V2-METHODOLOGY-1.
- **Per-§ ACCEPT rate at V3**: §2.1 P1-A 4/4 sub-axes ACCEPT;
  §2.2 P1-B 5/5 sub-axes ACCEPT; §2.3 P1-C 5/5 sub-axes ACCEPT;
  §2.4 P1-D 4/4 sub-axes ACCEPT. The V1 31-row base matrix + V2
  18-row disclosure matrix all recompute ACCEPT.
  **V3 aggregate sub-axis ACCEPT rate: 49/49 = 100 %.**

CH4 V3 vote: **ACCEPT** (zero orphan REVISEs; ≥95 % gate cleared
at 100 %). Combined with V2 100 % ACCEPT, this is the second of two
consecutive clean cycles required by §3Z. **CH4 lens convergence
LOCKED at V3.** Zero blocker into the V3 aggregator.

## §7 — Sources (every cite re-verified this turn)

- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` §0-§5 (V3 dispatch authority; inherited from V1+V2).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH4.md` (V1 baseline; CF-1..CF-4 + §4 fold recommendations 1-5; 93.5 % aggregate).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH4.md` (V2 confirming baseline; CF-1 closed; 100 % aggregate; CF-V2-1..4 ACCEPT-with-note carry-forward).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md` (V2 aggregator; binding for V3 baseline).
- `restart/prompts/skinny/PASS-1-PROFILE.md §3 CH4` (lens definition; binding).
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH4 def), §3Z (convergence rule).
- V2 artefacts (unchanged between V2 commit `069ba203c` and HEAD `4ad8f1949`):
  - `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` `:10` (build_flags_regime row + V1 correction + Cargo.toml `:78-86` cross-check + aggregator refusal cite).
  - `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` `:10`, `:185` (build_flags_regime row + cohort + §3 Build-flags regime guard paragraph + canonical refusal target named at results-table boundary).
  - `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` `:17-25` (build_flags_regime row + cohort delineation + Option A binding cite + illustrative refusal target).
  - `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` `:21-31` (build_flags_regime row + two-target-dir re-confirmation + freshness verification).
- Cargo manifest cross-check:
  - `skinny/Cargo.toml:78-86` `[profile.release]` block — recomputed this turn; carries `opt-level=3, lto="fat", codegen-units=1, panic="abort", debug=true, strip=false, split-debuginfo="packed"`; carries NO `target-cpu` line; `grep "target-cpu" skinny/Cargo.toml` returns zero matches. P1-A's V2 correction stands at V3.
- Host-side verification:
  - `git rev-parse HEAD` → `4ad8f1949099829b7ad723ddfd7eeb2a40cf61cd` (HEAD one commit past V2 commit; the V2 P1 frontmatters were not touched by `4ad8f1949`).
  - `git log --oneline -1 069ba203c` → `docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan REVISEs landed`.
  - `wc -l` on the four V2 P1 frontmatters → P1-A 343, P1-B 323, P1-C 616, P1-D 669 (byte-identical to V2 CH4 §7).
  - `grep -c "build_flags_regime"` on the four V2 P1 frontmatters → P1-A 1, P1-B 2, P1-C 2, P1-D 2 (byte-identical to V2 CH4 §7).
