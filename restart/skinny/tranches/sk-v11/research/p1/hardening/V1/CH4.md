# SK-V11 S-P1 CHALLENGE V1 CH4 Cost/Reproducibility

Disposition: REVISE.

CH4 asks whether the profile can be rerun by a third party: every method block
needs run id, host triple, build flags, and exact commands or clear
parameterization. The universal orchestrator also requires cost/wave alignment
and a realistic hard cap, but the S-P1 specialization makes reproducibility the
blocking question here.

## Read Scope

- `restart/prompts/skinny/PASS-1-PROFILE.md:143` requires verbatim rerunnable
  commands; `:145` says absence of run id, host triple, or build flags fails
  CH4.
- `restart/prompts/ORCHESTRATOR.md:86` defines universal CH4 COST, and
  `:112` through `:116` requires CHALLENGE findings to fold into the next pass
  cycle.
- W0 baseline, all six P1 artifacts, and `/tmp/skv11-p1` capture status/logs
  were read.

## Acceptable Reproducibility Evidence

- W0 is reproducible: it names commit `3ce75df4`, Criterion root, target root,
  run id, capture command, and gate verification command at
  `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:11` through
  `:18`.
- P1-B has the strongest product-plane method block. It names commit/run id,
  host triple, build flags, profile target, tool versions/sources, and clear
  parameterized commands at
  `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:8`
  through `:15` and `:24` through `:54`.
- P1-D gives rerunnable PMU/xctrace command shapes and the numeric authority:
  `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:8` through
  `:16`, `:20` through `:58`, and `:240` through `:253`.
- P1-F is reproducible as a document/results extraction: it records the current
  and prior run ids and exact extraction commands at
  `restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:20`
  through `:42`, with source list at `:202` through `:214`.
- Artifact paths are present and internally consistent. P1-A cites parse
  samply, xctrace, and PMU paths at
  `restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:156`
  through `:163`; P1-B cites direct/typed paths at
  `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:273`
  through `:285`; P1-D cites PMU and trace paths at
  `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:245`
  through `:251`.
- `xctrace rc=54` is explained sufficiently for reproducibility. P1-A says it
  is the 1000 ms time-limit path with saved trace output at
  `restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:66`
  through `:68`; P1-D records 81/82 CPU Counter and 81/82 Time Profiler rows
  as `rc=54`, with retained trace bundles, at
  `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:216`
  through `:223`.
- The samply symbolication caveat is disclosed rather than hidden. P1-A records
  `symbolicated=false` with sidecar `.json.syms.json` files at
  `restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:70`
  through `:73`; P1-B makes xctrace the symbol authority at
  `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:71`
  through `:76`; P1-E repeats the caveat at
  `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:263`
  through `:266`.

## Findings Requiring Fold

1. P1-A is not fully rerunnable as written. It says the exact samply command
   line was not embedded in the logs and that the listed form is a "command
   shape, not a verbatim transcript" at
   `restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:35`
   through `:37`. It also says the exact samply version was not recorded at
   `:13` through `:15`. This violates the PASS-1 CH4 requirement for verbatim
   rerunnable method blocks.

2. P1-C is reproducible as a W0 Criterion extraction, but not as the requested
   "samply mode III" profile. Its frontmatter says the profile tool is W0
   Criterion artifacts and explicitly says no new samply call-stack capture is
   claimed at
   `restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md:12`
   through `:14`. Its W0 commands are exact at `:21` through `:29`, and the run
   id appears later at `:176` through `:180`, but the artifact title/scope and
   prompt output row imply samply coverage. This needs either a real Mode III
   samply capture or a renamed/reframed W0 masking-probe extraction.

3. P1-E omits the run id in its own frontmatter and source list. It has baseline
   commit, host triple, build flags, profile sources, and coverage at
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:8`
   through `:13`, but no explicit run id appears in Section 5 at `:295`
   through `:330`. Because P1-E is the synthesis artifact that downstream
   consumers will cite, the run id must be stated directly there, not only in
   sibling artifacts.

4. Build provenance for `profile_direct` and `xctrace_probe` is ambiguous. P1-A
   declares baseline commit `3ce75df4` at
   `restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:8`
   through `:9`, but its commands use binaries under
   `/tmp/skv11-profile-target-9c8da194` at `:45` and `:52`. P1-B and P1-D use
   the same target root at
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:11`
   through `:12` and
   `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:24`
   through `:27`. P1-F distinguishes the measured W0 capture commit
   `3ce75df4` from documentation freeze commit `9c8da194` at
   `restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:157`
   through `:160`. Since `3ce75df4` is the `profile_direct` PMU commit, V1 must
   say which source SHA actually produced both binaries.

## Required Fold Into V2

- Add a shared capture-provenance block to every P1 artifact: run id, capture
  root, host triple, rust toolchain, build profile, `RUSTFLAGS`, source SHA used
  for `profile_direct` and `xctrace_probe`, documentation freeze SHA if
  different, target directory, binary paths, and exact build command.
- For P1-A, add `samply --version` and either the exact per-row samply transcript
  or the exact script/loop that generated all 34 commands. If the transcript
  cannot be recovered, mark samply as artifact-only evidence and keep xctrace as
  the self-time authority.
- For P1-C, either capture and cite actual Mode III samply artifacts/logs or
  retitle/scope it as W0 Criterion masking-probe extraction; in both cases move
  the run id into the frontmatter.
- For P1-E, state the run id explicitly in frontmatter and Section 5.
- Preserve the `rc=54` time-limit explanation and the `symbolicated=false`
  samply caveat in the consolidated hardening fold.

No CH4 REJECT is issued because the numeric/source artifacts exist, the
xctrace and PMU authorities are identified, and the disclosed caveats prevent a
false clean-close claim. The V1 set cannot be ACCEPTed until the provenance and
method gaps above are folded.
