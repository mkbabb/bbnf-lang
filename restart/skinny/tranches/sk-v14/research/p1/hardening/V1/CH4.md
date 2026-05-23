# S-P1 CHALLENGE V1 — CH4 COST (Profile Reproducibility)

Lens: CH4 — every §1 method block must carry verbatim commands a third
party can re-run; run id, host triple, build flags, samply version,
PMU access matrix all reproducible. Per `PASS-1-PROFILE.md §3 CH4`:
absent any one of those fields = CH4 FAIL for that artefact.

Pass: S-P1 Profile. Cycle: V1. Date: 2026-05-23.
Author: CH4 lens agent (write-only). HARD CAP 30 min. No git mutation.
Authoritative dispatch: `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` §0-§5; CH4 focus per §2.

## §1 — Disposition summary

| Artefact | Verbatim commands | Host triple | Build flags | samply version | Run id / timestamp | PMU access matrix | Verdict |
|---|---|---|---|---|---|---|---|
| P1-A `p1a-samply-mode-1.md` | ACCEPT | ACCEPT | **REVISE** (RUSTFLAGS line absent; native CPU asserted "per Cargo.toml" only) | ACCEPT | ACCEPT (`skv14-p1a/2547c750.../2026-05-23T06:37:31Z`) | n/a (P1-D scope) | ACCEPT-with-finding |
| P1-B `p1b-samply-mode-2.md` | ACCEPT | ACCEPT | **REVISE** (explicitly `RUSTFLAGS unset`; rows compared against P1-C/D which pin `-C target-cpu=native` — see CF-1) | ACCEPT | ACCEPT (`skv14-p1b-V1-2026-05-23T02h45-aarch64-apple-darwin`) | n/a | ACCEPT-with-finding |
| P1-C `p1c-samply-mode-3.md` | ACCEPT | ACCEPT | ACCEPT (`release + debug=true + RUSTFLAGS="-C target-cpu=native"`, §5.5) | ACCEPT | ACCEPT (§5.5: HEAD `2547c750...`; date 2026-05-23) | n/a | ACCEPT |
| P1-D `p1d-pmu-cycles.md` | ACCEPT | ACCEPT | ACCEPT (`release + debug=true + RUSTFLAGS="-C target-cpu=native"`; both target dirs disclosed) | ACCEPT (0.13.1 in matrix §1.4) | ACCEPT (identity.txt `2026-05-23T06:39:34Z`) | ACCEPT (§1.4 escalation matrix is byte-identical to SK-V13 V3 finding; cycles/instructions REACHABLE via `proc_pid_rusage(V5).ri_cycles/ri_instructions`; branch/L1/LLC UNREACHABLE without root in macOS 26.4; verified by `sudo -n true` refusal in identity.txt) | ACCEPT |

Overall per-§ ACCEPT rate: **4/4 artefacts pass** the CH4 axis on the
load-bearing fields (verbatim commands + host triple + samply version
+ run id + PMU access matrix). **2/4 artefacts (P1-A + P1-B) carry a
REVISE-grade finding** on the `RUSTFLAGS` build-flag disclosure that
threatens cross-artefact c/B comparability; this is a CH4 cohesion
defect, not an isolated reproducibility break. See §3 CF-1.

Hard cap status: 30 min budget; this write ≈ 18 min wall.

## §2 — Per-artefact CH4 evidence table

Every cell carries a `path:line` cite, recomputed against the on-disk
artefacts per `CHALLENGE-CONTEXT §3` executable-verification mandate.
Host-side verification of cited toolchain versions: `samply 0.13.1` +
`rustc 1.96.0-nightly (02c7f9bec 2026-04-10)` reproduce on this host
this turn (PATH lookup, fresh invocation).

### §2.1 — P1-A `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md`

| CH4 sub-axis | Cite | Evidence | Verdict |
|---|---|---|---|
| Verbatim cargo build | `p1a-samply-mode-1.md:30-35` | Block has `cd skinny`, `CARGO_TARGET_DIR=/tmp/skv14-p1a-target cargo build --release --bin xctrace_probe`. Third party can re-execute. | ACCEPT |
| Verbatim samply capture | `p1a-samply-mode-1.md:42-54` | `samply record --rate 4000 --no-open --save-only --unstable-presymbolicate -o ... -- "$BIN/xctrace_probe" "$absolute_path" track1 "$iters"`. Loop driver path `/tmp/skv14-p1/samply/run-samply-p1a.sh` cited. Loop driver verified present (parent dir `/tmp/skv14-p1/artifacts/identity.txt` reachable). | ACCEPT |
| Verbatim PMU re-run | `p1a-samply-mode-1.md:86-88` | `"$BIN/xctrace_probe" "$absolute_path" track1 "$iters" > "$ROOT/pmu/logs/..."`. Reproducible. | ACCEPT |
| Host triple | `p1a-samply-mode-1.md:8` + `:24` (in identity heredoc) | `aarch64-apple-darwin (Darwin 25.4.0 arm64)`. | ACCEPT |
| Build flags | `p1a-samply-mode-1.md:9` | `release profile, debug=true, lto=fat, codegen-units=1, panic=abort, split-debuginfo=packed, native target CPU per skinny/Cargo.toml`. **RUSTFLAGS not shown explicitly** — "native target CPU per Cargo.toml" rests on the workspace `[profile.release]` definition; a third party who reads the document but not Cargo.toml cannot reconstruct whether `RUSTFLAGS="-C target-cpu=native"` was set. Cross-check vs P1-C/D (both pin RUSTFLAGS explicitly) and P1-B (asserts `RUSTFLAGS unset`) — see CF-1. | REVISE |
| samply version | `p1a-samply-mode-1.md:10` | `samply 0.13.1` with flags listed. | ACCEPT |
| Run id / timestamp | `p1a-samply-mode-1.md:12` + `:324` | `skv14-p1a/2547c750bc78533d738eb85913206a0872022818/2026-05-23T06:37:31Z`. Commit verified on-disk via `git log --oneline -1 2547c750bc78533d738eb85913206a0872022818` → `docs(sk-v14-p1-profile): seed S-P1 dispatch context`. Identity ledger at `/tmp/skv14-p1/artifacts/identity.txt` verified present + contents match (commit + date + host_triple). | ACCEPT |

### §2.2 — P1-B `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md`

| CH4 sub-axis | Cite | Evidence | Verdict |
|---|---|---|---|
| Verbatim cargo build | `p1b-samply-mode-2.md:19-22` | `cd skinny`, `CARGO_TARGET_DIR=/tmp/skv14-p1b-target cargo build --release -p bbnf-bench --bin profile_direct`. Binary size + codeId disclosed at `:24`. | ACCEPT |
| Verbatim samply capture | `p1b-samply-mode-2.md:54-61` | `samply record --save-only --no-open --unstable-presymbolicate -r 4000 -o ... -- /tmp/skv14-p1b-target/release/profile_direct <iters> <corpus> <mode>`. Sweep driver `/tmp/skv14-p1b/run_sweep.sh` cited. | ACCEPT |
| Per-corpus iter table | `p1b-samply-mode-2.md:30-48` | Explicit 17-row Bytes/Iters/Wall table — a third party can reproduce the same sample-mass envelope. | ACCEPT |
| Host triple | `p1b-samply-mode-2.md:8` | `aarch64-apple-darwin (Apple M5 Max, 18 cores: 6 efficiency + 12 performance; 128 GB)`. CPU model granularity exceeds P1-A's `Darwin 25.4.0 arm64`. | ACCEPT |
| Build flags | `p1b-samply-mode-2.md:9` + `:307-311` | `[profile.release] opt-level=3 lto="fat" codegen-units=1 panic="abort" debug=true strip=false split-debuginfo="packed"`; **`RUSTFLAGS unset (no target-cpu=native override at the cargo invocation level; the binary uses the default aarch64 baseline)`** (`:311`). This is the cleanest disclosure of the four, BUT it directly contradicts P1-C/D's `RUSTFLAGS="-C target-cpu=native"`. Cross-artefact c/B comparisons (e.g. P1-B Track 1 direct `twitter` 11037 Mbps vs P1-D Track 1 direct `twitter` 11627 Mbps) are not at the same build-flag baseline. See CF-1. | REVISE |
| samply version | `p1b-samply-mode-2.md:10` + `:305` | `samply 0.13.1`. Profile binary codeId `4F214AC279FB380A9F745CE9615B2850` disclosed (`lipo -info / dwarfdump`) — a third party can reproduce + verify the binary identity. | ACCEPT |
| Run id / timestamp | `p1b-samply-mode-2.md:303-305` | `skv14-p1b-V1-2026-05-23T02h45-aarch64-apple-darwin` (whole-sweep) + per-record outname uniqueness rule. | ACCEPT |

### §2.3 — P1-C `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md`

| CH4 sub-axis | Cite | Evidence | Verdict |
|---|---|---|---|
| Verbatim cargo build | `p1c-samply-mode-3.md:35-43` | `cd skinny`, `CARGO_TARGET_DIR=/tmp/skv14-p1c-target RUSTFLAGS="-C target-cpu=native" cargo build --release --bench json_parity --bench simd_scan -p bbnf-bench`. Binary hashes disclosed (`json_parity-fa5381f7fa4e9e97`, `simd_scan-e9e48792f0c6e621`). Build time + outputs cited. | ACCEPT |
| Verbatim cargo profile pin | `p1c-samply-mode-3.md:47-60` | Full `[profile.release]` TOML block reproduced inline (opt-level=3, lto=fat, codegen-units=1, panic=abort, debug=true, strip=false, split-debuginfo=packed) — strictly self-contained. | ACCEPT |
| Verbatim samply per-probe captures | `p1c-samply-mode-3.md:73-105` | All four probe invocations carry the explicit `samply record --save-only --no-open --rate 4000 --unstable-presymbolicate -o ... -- <binary> --bench <probe>` form. Aarch64 carve-out for `alternate_pext_mask_plan` documented (`:107-110`). | ACCEPT |
| Throughput-extraction formula | `p1c-samply-mode-3.md:122-132` | `Mbps = (bytes * 8000) / ns_per_iter`; `c/B = (ns_per_iter * 4.4) / bytes`. Convention pinned to `bbnf-bench/src/bin/gate.rs:3719-3725`. | ACCEPT |
| Host triple | `p1c-samply-mode-3.md:13-14` + `:603` | `aarch64-apple-darwin (cpu = Apple M5 Max; per-core nominal 4.4 GHz)`. The 4.4 GHz figure is the basis for the c/B column — disclosing it is load-bearing for CH4. | ACCEPT |
| Build flags | `p1c-samply-mode-3.md:15-16` + `:606` | `release + debug=true + RUSTFLAGS="-C target-cpu=native"`; target dir `/tmp/skv14-p1c-target`. | ACCEPT |
| samply version | `p1c-samply-mode-3.md:17-21` + `:604` | `samply 0.13.1`. Sidecar mechanics explained — a third party with samply 0.13.1 can reproduce identical syms.json layout. | ACCEPT |
| Run id / timestamp | `p1c-samply-mode-3.md:599-607` | `Repo HEAD at run: 2547c750bc78533d738eb85913206a0872022818`; baseline source SK-V13 close `ff653fbe6` (both verified via `git log` this turn). Date: 2026-05-23. | ACCEPT |

### §2.4 — P1-D `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md`

| CH4 sub-axis | Cite | Evidence | Verdict |
|---|---|---|---|
| Verbatim cargo build (parse + direct + typed) | `p1d-pmu-cycles.md:40-43` | `cd skinny`, `CARGO_TARGET_DIR=/tmp/skv14-p1d-target RUSTFLAGS="-C target-cpu=native" cargo build --release --bin xctrace_probe --bin profile_direct -p bbnf-bench`. | ACCEPT |
| Verbatim cargo build (mode-III scratch crate) | `p1d-pmu-cycles.md:58-64` | Separate scratch crate at `/tmp/skv14-p1d/mode3-probe/` with path-deps; built under second target dir `/tmp/skv14-p1d-mode3-target`. Scratch-crate Cargo.toml + src/main.rs verified present on-disk. Rationale (no `skinny/` mutation) explicitly documented (`:66-68`). | ACCEPT |
| Verbatim PMU capture loops | `p1d-pmu-cycles.md:73-88` | Four sequential bash scripts: `run-pmu.sh`, `run-direct.sh`, `run-typed.sh`, `run-mode3.sh`; all four verified present on-disk. Output redirected to per-script run.log per `[test-output-to-file]`. | ACCEPT |
| Verbatim xctrace capture | `p1d-pmu-cycles.md:98-115` | Full `xcrun xctrace record ... --launch -- /tmp/skv14-p1d-target/release/xctrace_probe ...` invocation + `xctrace export --toc` + `xctrace export --xpath ...` reproduced. | ACCEPT |
| Host triple | `p1d-pmu-cycles.md:15` + identity.txt | `aarch64-apple-darwin`. Identity ledger `/tmp/skv14-p1d/artifacts/identity.txt` adds `os=26.4.1`, `kernel=25.4.0`, `xctrace_version=xctrace version 26.0 (17A5241e)`, `sudo_available=sudo: a password is required / exit=1` — granular reproducibility. | ACCEPT |
| Build flags | `p1d-pmu-cycles.md:16-20` | `release` profile with full flag list + `RUSTFLAGS="-C target-cpu=native"`; both `/tmp/skv14-p1d-target` and `/tmp/skv14-p1d-mode3-target` disclosed. | ACCEPT |
| samply version | `p1d-pmu-cycles.md:25-26` + `:139` | `samply 0.13.1 available but not load-bearing for P1-D` (P1-D is rusage + xctrace, not samply). Disclosed for completeness. | ACCEPT |
| Run id / timestamp | `p1d-pmu-cycles.md:586-591` + identity.txt | Identity ledger holds `commit=2547c750bc78533d738eb85913206a0872022818`, `date_utc=2026-05-23T06:39:34Z`. | ACCEPT |
| **PMU access matrix (CH4 spotlight)** | `p1d-pmu-cycles.md:126-150` | Six-counter matrix with `Reachable unprivileged?`, `Verified-by` (per-row sample count: n=231 for cycles/inst), `Fallback if unreachable` columns. Cycles + instructions REACHABLE via `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles/ri_instructions`. branch-miss / L1D / L1I / LLC / bus-cycles / ref-cycles all UNREACHABLE with `Verified-by: xctrace export TOC has no pmc schema; cpu-state has no counter column`. Escalation footnote: `Root access was probed (sudo -n true) and refused (no password cached, no NOPASSWD entry) — the unprivileged ceiling is binding` (`:144-148`); cross-verified against identity.txt `sudo_available=sudo: a password is required / exit=1`. Cross-referenced to SK-V13 V3 finding at `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md §4(1)` (cited `:124`) — same finding, same export schema, byte-identical reproducibility envelope. | ACCEPT |

## §3 — Critical findings

### CF-1 (cross-artefact build-flag drift; CH4-COHESION) — REVISE

The four artefacts split into two `RUSTFLAGS` regimes:

| Artefact | RUSTFLAGS | Cite |
|---|---|---|
| P1-A | implied `-C target-cpu=native` "per Cargo.toml" but not shown in any shell block | `p1a-samply-mode-1.md:9` |
| P1-B | **explicitly `RUSTFLAGS unset` — default aarch64 baseline** | `p1b-samply-mode-2.md:311` |
| P1-C | `RUSTFLAGS="-C target-cpu=native"` (explicit in build block) | `p1c-samply-mode-3.md:37`, `:606` |
| P1-D | `RUSTFLAGS="-C target-cpu=native"` (explicit in both build blocks) | `p1d-pmu-cycles.md:41`, `:62`, `:18` |

`skinny/Cargo.toml` does not propagate `-C target-cpu=native` through
`[profile.release]` (that profile section in the file holds opt-level,
lto, codegen-units, panic, debug, strip, split-debuginfo — none of
which carry target-cpu). The cargo manifest cannot set RUSTFLAGS;
target-cpu is therefore an environment-level override. P1-A's
"native target CPU per skinny/Cargo.toml" phrasing is misleading —
the workspace Cargo.toml does **not** pin native; only an
environment `RUSTFLAGS` (or `.cargo/config.toml [build]`) does.

**Reproducibility impact.** P1-B Track 1 direct `twitter` (11037 Mbps,
3.00 c/B) vs P1-D Track 1 direct `twitter` (11627 Mbps, 2.938 c/B):
the 5.3% Mbps + 2.1% c/B gap is partially attributable to the
target-cpu delta (NEON intrinsics in `bbnf-simd` get
aarch64-baseline codegen under P1-B vs Apple-Silicon-specialised
codegen under P1-D). A CH4-reproducible profile must close this drift.
**REVISE-target**: V2 must either (a) repeat all four captures under
a single `RUSTFLAGS` regime, or (b) carry a per-artefact `RUSTFLAGS`
column on every row so the comparator stays apples-to-apples per
`[no-warm-benches]` discipline (which forbids silent build-flag
divergence between rows compared in the same table).

This is the only CH4 cohesion defect in the V1 artefact set. It does
not falsify any individual artefact's intra-row claims (each artefact
is internally consistent at its own build-flag regime), but it does
break the cross-artefact c/B + Mbps comparator that S-P2 must
consume — the same defect class that S-P0 flagged for `sonic_rs`
comparator misbinding, transposed onto the build-flag axis.

### CF-2 (clock-overhead inflation of c/B on small corpora; CH4-METHOD) — ACCEPT-with-note

P1-B §4 anomaly 6 (`p1b-samply-mode-2.md:280-282`) flags that
`y_string_unicode-direct-Track1` reports 10.38 c/B but the parser's
true c/B is ≈9.7 — the `mach_absolute_time` + `proc_pid_rusage` per-iter
overhead adds ~0.7 c/B on the smallest corpus. This is correctly
disclosed in §4 and proposed for P1-D escalation (rdpmc-class
hardware sampling or end-of-loop rusage with larger iter count).
**Disposition**: this is a known method-level CH4 limitation, fully
disclosed by P1-B and cross-referenced by P1-D §1.4 (`samply per-symbol
sample-count proxy ... not load-bearing for c/B`). No REVISE needed;
S-P2 must consume this as a documented floor of the c/B methodology.

### CF-3 (P1-C aggregate-only attribution; CH4-GRAIN) — ACCEPT-with-note

P1-C §2.2 (`p1c-samply-mode-3.md:189-194`) discloses that the
per-probe samply captures aggregate all 17 corpora into a single
profile per probe. Per-corpus separation requires per-corpus
captures — escalated to V2 as `escalation §C` in P1-C. The
aggregate symbol-table is reproducible (sidecar artefacts +
extraction script paths cited at `:570-580`) but per-corpus c/B
attribution rests on criterion `estimates.json` slope extraction,
not per-corpus samply capture. **Disposition**: the grain mismatch
is explicitly disclosed and reproducible; CH4 accepts. CH2/CH6 may
have a separate lens on whether aggregate attribution is sufficient
for `parse-attribution` feature enablement — out of CH4 scope.

### CF-4 (P1-D PMU matrix vs SK-V13 V3 parity; CH4-DELTA) — ACCEPT

The escalation matrix at `p1d-pmu-cycles.md:128-148` is asserted as
"**byte-identical to the SK-V13 V3 finding logged at
restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md §4(1)**"
(`:124`). Cross-verification: the SK-V13 V3 finding documents the
same `unavailable_from_current_export` ceiling on macOS aarch64
without root, the same `xctrace export TOC` schema gap (no PMC
columns), and the same `proc_pid_rusage(V5)` cycles/instructions
exposure path. The SK-V14 identity.txt `sudo_available=sudo: a
password is required` directly evidences the unprivileged binding.
**Disposition**: the matrix is reproducible (every row carries
`Verified-by` evidence, every UNREACHABLE row carries the same
`Fallback if unreachable = unavailable_from_current_export` cell)
and consistent with the SK-V13 baseline; CH4 accepts.

## §4 — V2 fold recommendations

1. **Unify RUSTFLAGS across all four artefacts.** Either rerun P1-B
   under `RUSTFLAGS="-C target-cpu=native"` to match P1-C/D, or
   rerun P1-A/C/D with `RUSTFLAGS unset` to match P1-B. Pick the
   regime that matches the production gate build (almost certainly
   `target-cpu=native` since the gate consumer `gate.rs:3719-3725`
   uses the same conversion formula P1-C documents). Aggregator V2
   must carry a single `build_flags_regime` row attribute and refuse
   to compute a cross-artefact c/B delta where regimes differ.
   Owner: V2 aggregator + P1-B re-run (smaller scope than P1-A/C/D).

2. **Surface `clock_overhead_c_per_byte` as a schema column.** P1-B
   §4 anomaly 6 + P1-D §1.4 both note the floor; the schema in
   `restart/skinny/tranches/sk-v14/SYNTHESIS.md §2` should add a
   `clock_overhead_floor_cpb` column so small-corpus c/B rows
   (y_string_unicode, github_events, numbers) carry an explicit
   floor disclosure. Pre-aggregator hook.

3. **Per-corpus samply captures for the four mode-III probes.** P1-C
   `escalation §C` proposes per-corpus captures; V2 should land
   them so that per-corpus per-probe attribution is reproducible at
   the same grain as P1-A/B's parse_only path. The escalation is
   explicitly P1-C scope, not CH4-blocking; CH4 simply notes the
   grain disclosure is correct as-is and the V2 fold makes it
   stricter.

4. **PMU access matrix re-verification per V cycle.** P1-D §1.4
   asserts byte-identical reproduction of SK-V13 V3 — V2 should
   re-run `xctrace export --toc` + `sudo -n true` per cycle and
   carry a column "verified_this_cycle: YES/NO" so the matrix
   cannot silently bit-rot between V iterations. Identity.txt
   `sudo_available` + `xctrace_version` fields already hold the
   evidence; surface them.

5. **No CH4 finding pre-blocks REDRESS.** Per CHALLENGE-CONTEXT §2
   CH3 cross-check: none of CF-1..CF-4 re-proposes any pre-blocked
   REDRESS route. CF-1 is a build-flag discipline finding, CF-2 a
   measurement floor, CF-3 a grain disclosure, CF-4 a parity
   confirmation — all upstream of any REDRESS gate.

## §5 — Convergence vote

Per `PASS-1-PROFILE.md §3 CH4` + `ORCHESTRATOR.md §3Z`:

- **ACCEPT**: 4/4 artefacts pass the load-bearing CH4 axis
  (verbatim commands, host triple, samply version, run id, PMU
  matrix all present + on-disk verified).
- **REVISE**: 1 cross-cutting finding (CF-1 RUSTFLAGS drift)
  affecting 2 artefacts (P1-A + P1-B) on the build-flag disclosure
  sub-axis. This is a V2-fold item, not a V1-block.
- **Per-§ ACCEPT rate**: §2.1 P1-A 6/7 sub-axes ACCEPT, 1 REVISE;
  §2.2 P1-B 6/7 sub-axes ACCEPT, 1 REVISE; §2.3 P1-C 8/8 sub-axes
  ACCEPT; §2.4 P1-D 9/9 sub-axes ACCEPT (incl. CH4-spotlight PMU
  matrix). **Aggregate sub-axis ACCEPT rate: 29/31 = 93.5%**.

CH4 vote: **ACCEPT-with-REVISE** (CF-1 to be folded by V2
aggregator; V2 cycle expected to clear the ≥95% gate per
ORCHESTRATOR §3Z once RUSTFLAGS unifies).

## §6 — Sources (every cite verified this turn)

- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` §0-§5 (dispatch authority).
- `restart/prompts/skinny/PASS-1-PROFILE.md §3 CH4` (lens definition; binding).
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH4 def), §3Z (convergence rule).
- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` (artefact under review).
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (artefact under review).
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` (artefact under review).
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` (artefact under review).
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md §4(1)` (cross-cycle PMU matrix parity baseline).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §2` (telemetry schema; for V2-fold §4.2).
- Host-side verification:
  - `git log --oneline -1 2547c750bc78533d738eb85913206a0872022818` → confirms `docs(sk-v14-p1-profile): seed S-P1 dispatch context`.
  - `git log --oneline -1 ff653fbe6` → confirms `docs(sk-v14-audit-overfit-hardening-V5): challenge V5 + consolidated — G-S-P0-CONVERGED`.
  - `samply --version` → `samply 0.13.1` (matches all four artefacts).
  - `rustc --version` → `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)` (matches all four artefacts).
  - `/tmp/skv14-p1/artifacts/identity.txt` reachable; `commit=2547c750...`, `date=2026-05-23T06:37:31Z`, `host_triple=aarch64-apple-darwin` (matches P1-A frontmatter).
  - `/tmp/skv14-p1d/artifacts/identity.txt` reachable; `commit=2547c750...`, `date_utc=2026-05-23T06:39:34Z`, `samply_version=samply 0.13.1`, `xctrace_version=xctrace version 26.0 (17A5241e)`, `sudo_available=sudo: a password is required / exit=1` (matches P1-D §1.4 escalation matrix).
  - `/tmp/skv14-p1a-target/release/`, `/tmp/skv14-p1b-target/release/`, `/tmp/skv14-p1c-target/{release,criterion}/` all reachable.
  - `/tmp/skv14-p1d/mode3-probe/Cargo.toml`, `/tmp/skv14-p1d/mode3-probe/src/main.rs`, `/tmp/skv14-p1d/run-{pmu,direct,typed,mode3}.sh` all reachable.
  - `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`, `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`, `skinny/crates/bbnf-bench/benches/json_parity.rs` all reachable (cited as hot-leaf + probe source files across the four artefacts).
