# SK-V9 S-P1 Hardening V1 CH4: Cost And Reproducibility

Disposition: REVISE.
Confidence: 88%.

Scope read:

- `restart/audit/pass-1-substrate/PASS-1.md`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md`

## CH4 Verdict

The packet is not a reproducible S-P1 profile yet. It is mostly an honest
opening-authority and gap ledger: the files do not fabricate fresh samply
symbols, PMU counters, cycles-per-byte, or SK-V9-open row deltas. That restraint
is important and should be preserved.

However, CH4 asks whether the profile can be replayed. Under the S-P1 contract,
every method block must carry verbatim rerunnable commands, and a profile whose
run id, host triple, or build flags are absent fails CH4
(`restart/prompts/skinny/PASS-1-PROFILE.md:143`-
`restart/prompts/skinny/PASS-1-PROFILE.md:146`). The current packet repeatedly
uses the W0 `SK-V8-open` run id as opening authority while declaring fresh
SK-V9-open samply/PMU evidence absent. That can support a REVISE gap fold; it
cannot support ACCEPT or S-P2 handoff.

If the consolidation tries to advance S-P1 as a completed empirical profile,
upgrade this disposition to REJECT.

## Defects

1. Critical: fresh run identity is absent for the actual profile claims.
   P1-A, P1-B, P1-C, and P1-D all say fresh SK-V9-open samply or PMU evidence is
   absent while using the W0 `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`
   authority (`p1a-samply-mode-1.md:7`-`p1a-samply-mode-1.md:11`;
   `p1b-samply-mode-2.md:7`-`p1b-samply-mode-2.md:11`;
   `p1c-samply-mode-3.md:7`-`p1c-samply-mode-3.md:11`;
   `p1d-pmu-cycles.md:7`-`p1d-pmu-cycles.md:11`). This is a valid absence
   record, but not a reproducible profile run under CH4.

2. Critical: PMU and c/B evidence cannot be replayed because no PMU command,
   counter log, or admitted PMU artifact exists. P1-D correctly refuses to derive
   c/B from ns/B and says cycles are missing (`p1d-pmu-cycles.md:71`-
   `p1d-pmu-cycles.md:73`, `p1d-pmu-cycles.md:271`-
   `p1d-pmu-cycles.md:284`). The fold must preserve that refusal and block any
   cost-model claim that needs cycles-per-byte until same-run `cycles /
   input_bytes` exists.

3. Major: some method blocks are not verbatim enough to replay. P1-B contains
   an `awk` command with `{ ... }`, which is explicitly not a runnable command
   (`p1b-samply-mode-2.md:26`-`p1b-samply-mode-2.md:38`). P1-F provides the two
   `git diff --quiet` checks but not the exact extraction command that produced
   its 38-row table (`p1f-results-delta.md:19`-`p1f-results-delta.md:27`,
   `p1f-results-delta.md:44`-`p1f-results-delta.md:83`). P1-C records an
   `absent:` token for commands intentionally not run, but no command proving the
   cited `/tmp/skv9-p1` profile paths are absent (`p1c-samply-mode-3.md:34`-
   `p1c-samply-mode-3.md:40`, `p1c-samply-mode-3.md:141`-
   `p1c-samply-mode-3.md:148`).

4. Major: host/build metadata is not same-run with the missing fresh profile
   evidence. The artifacts often cite W0 manifest host/build fields while saying
   the actual samply or PMU run is absent. That is acceptable only if the fold
   labels them as `opening_authority_host_build`, not as fresh profile
   reproducibility. Fresh profiling must capture host triple, CPU/features,
   build flags, rustc/cargo version, profile tool version, target directory, and
   run id in the same manifest as the measurements.

5. Major: abbreviated SHAs weaken checkout replay. P1-E uses
   `b258a406ff7f`, and P1-F uses `b258a406` plus prior close `32870fea`
   (`p1e-hot-leaf-attribution.md:8`-`p1e-hot-leaf-attribution.md:9`;
   `p1f-results-delta.md:7`-`p1f-results-delta.md:10`). Use full 40-character
   commit IDs in frontmatter and sources. Short SHAs are not enough for a
   durable replay record.

6. Major: Criterion slope identifiers are not archival profile artifacts.
   Current rows cite logical strings such as
   `criterion-slope-profile:json_<corpus>/<bench>/new/estimates.json`, while the
   actual files live under local target output if present. S-P1 permits profile
   binaries outside the doc tree, but P1 artifacts must cite their paths and run
   ids (`restart/prompts/skinny/PASS-1-PROFILE.md:204`-
   `restart/prompts/skinny/PASS-1-PROFILE.md:206`). A rendered identifier alone
   is not enough to replay the measurement outside the local working copy.

7. Major: the packet cannot be folded into a 90-minute challenge wave as fresh
   telemetry. The pass budget allows 45 minutes per P1 agent, a 90-minute
   CHALLENGE wave, and about 2.5 hours per cycle
   (`restart/prompts/skinny/PASS-1-PROFILE.md:227`-
   `restart/prompts/skinny/PASS-1-PROFILE.md:237`). Running 17-corpus samply
   modes I/II/III plus PMU/masking-probe capture is data production, not a CH4
   doc fold. The fold must either stay docs-only or open a new telemetry-lock
   data cycle with its own cap/user extension.

8. Major: no explicit LOC budget note is attached to the P1 fold. S-P1 is
   read-only against `skinny/` source and writes only under the P1 output root
   (`restart/prompts/skinny/PASS-1-PROFILE.md:32`-
   `restart/prompts/skinny/PASS-1-PROFILE.md:33`,
   `restart/prompts/skinny/PASS-1-PROFILE.md:46`-
   `restart/prompts/skinny/PASS-1-PROFILE.md:49`). PASS-1 also defines a
   generated-code budget schema for later generated-output pressure
   (`restart/audit/pass-1-substrate/PASS-1.md:293`-
   `restart/audit/pass-1-substrate/PASS-1.md:303`). The consolidation needs an
   explicit note: this CH4 fold consumes 0 source LOC and 0 generated LOC; any
   gate/report/telemetry code change must be routed to the SK-V9 Alpha LOC
   budgets, not hidden inside P1 hardening.

9. Minor: the artifact set is inconsistent about method section naming. Some use
   `## §1`, others use `## Section 1`. That does not block replay by itself, but
   the fold should normalize section labels because the S-P1 schema names exact
   sections (`restart/prompts/skinny/PASS-1-PROFILE.md:88`-
   `restart/prompts/skinny/PASS-1-PROFILE.md:110`).

## Replay Assessment

| Artifact | Replayable now? | CH4 assessment |
|---|---|---|
| P1-A | Partially | Static extraction from committed docs is mostly replayable; the exact `perl` row extraction helps. Fresh samply is explicitly absent, so profile evidence is not replayable. |
| P1-B | No for full method | The gap ledger is inspectable, but the abbreviated `awk { ... }` command is non-runnable and no samply command/log exists. |
| P1-C | Partially | Source/gate inspection is replayable. Absence of `/tmp/skv9-p1` artifacts is asserted but not proven by a command in the method block. |
| P1-D | Partially | Row metadata extraction is replayable from `RESULTS.md`; PMU/c/B is correctly blocked and cannot be replayed. |
| P1-E | Partially | Source-surface classification is replayable by reading cited files; it does not replay a hot-leaf profile because no samply artifacts exist. |
| P1-F | Partially | The byte-identical SK-V8-close comparison is replayable from the two `git diff --quiet` commands; the large extraction table lacks a verbatim generation command. |

Net: the evidence can be replayed as a W0/SK-V8-open document extraction and
gap audit. It cannot be replayed as an SK-V9-open empirical profile.

## Fold Requirements

1. Preserve the top-line disposition as REVISE unless fresh SK-V9-open profile
   evidence is added. Do not let the consolidation treat absence-coded rows as
   completed S-P1 profile rows.

2. Add a packet-level replay manifest with full commit IDs, branch name, dirty
   state, date, host triple, CPU/features, rustc/cargo version, `samply --version`
   or PMU tool version, build flags, feature mask, `CARGO_TARGET_DIR`, and output
   directory. Split fields into `opening_authority_*` and `fresh_profile_*` so W0
   metadata cannot masquerade as same-run SK-V9-open metadata.

3. Replace every abbreviated command with runnable shell. In particular, expand
   the P1-B `awk` command, add the P1-F table extraction command, and add
   explicit absence checks for expected samply/PMU artifact directories.

4. For any fresh profile cycle, name the exact interactive `samply record`
   commands for modes I, II, and III with `debug=true`; name the PMU collection
   command; redirect stdout/stderr to durable log paths; and list every generated
   artifact path plus run id in §5.

5. For c/B, fold P1-D's derivation rule exactly: derive only from same-run
   `cycles / input_bytes`; reject any conversion from ns/B, CPU frequency,
   throughput, or sample cost.

6. Add a 90-minute wave note. CH4 doc fixes may fold inside the challenge wave;
   fresh 17-corpus samply/PMU execution must be a new P1 data cycle or an
   explicit user-approved extension, not silent challenge work.

7. Add a LOC budget note. This review/fold is docs-only: 0 source LOC,
   0 generated LOC, and no `skinny/` source writes. If telemetry-lock requires
   gate/report code, route it to the Alpha/S-P3 budgeted candidate rather than
   charging it to P1 hardening.

8. State the replay boundary in the consolidated verdict: W0 row extraction is
   replayable from committed docs and local Criterion cache if present; fresh
   SK-V9-open samply, PMU, hot-leaf, masking-probe, and c/B evidence is absent
   and blocks S-P1 convergence.
