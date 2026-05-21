# SK-V13 S-P1 V3 CH6: Anti-Paper-Close

Pass: S-P1 Profile. Cycle: V3.
Date: 2026-05-21.
Lens: CH6 ANTI-PAPER-CLOSE.
Disposition: ACCEPT.

## Verdict

ACCEPT. V3 fixes the CH6 paper-close defect from V2. It does not pretend that
save-only profiles, function-only sidecars, the CSS timer/fact-sink profile, or
the ten absent typed product rows are closed profile surfaces. It instead gives
each one an explicit non-closing status and keeps the entire cohort behind the
`profile_signal_not_gate_admission` boundary.

This is a CH6 acceptance of honesty and citable routing, not a declaration that
S-P1 now has interactive samply-quality symbol resolution for every row. The
remaining gaps stay available as profile signals only; they cannot be consumed
as gate admission or precise primitive closure without later recapture,
re-symbolication, CSS parser-leaf separation, or typed product-surface work.

## Review Basis

- `restart/prompts/skinny/PASS-1-PROFILE.md:155-160` requires that a
  self-report of "profiled" stand only on citable profile artefacts with
  resolvable symbols; `:251-254` names the save-only samply limitation.
- `restart/prompts/skinny/PASS-1-PROFILE.md:239-266` adds the cold,
  sequential, samply-discipline, masking-probe, and substrate-union axes CH6
  must police.
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md:45-73`
  required V3 to publish a canonical evidence ledger, preserve save-only
  symbolication as provisional where needed, keep mode-III function-only rows
  unresolved, keep CSS parser attribution unresolved, and keep missing typed
  rows as missing product surfaces.
- V3 support files read:
  `support/evidence-ledger-v3.md`,
  `support/profile-provenance-v3.md`.
- V3 agent artefacts read:
  `p1a-samply-mode-1.md`,
  `p1b-samply-mode-2.md`,
  `p1c-samply-mode-3.md`,
  `p1d-pmu-cycles.md`,
  `p1e-hot-leaf-attribution.md`,
  `p1f-results-delta.md`.

## Artefact Inventory Check

The citable artefacts still exist on disk:

| Surface | Profiles | Sidecars | CH6 read |
|---|---:|---:|---|
| V1 parse | 34 | 34 | citable save-only artefacts, still provisional |
| V1 typed | 14 | 14 | seven typed rows x two tracks only |
| V2 direct | 34 | 34 | non-panic direct evidence; one rank-1 timer/file-line gap |
| V2 mode III | 85 | 85 | full capture matrix, but rank-1 sidecar rows remain function-only |
| V2 CSS declaration-values | 1 | 1 | profile/log exist; rank-1 is timer overhead |

The V2 top-20 TSV confirms the remaining file-line gaps are not hidden: 85/85
mode-III rank-1 rows lack file or line in the TSV, 1/34 direct rank-1 rows lacks
file or line, and the CSS top three leaves are `mach_absolute_time`,
`LocalFactSink::finish`, and `FactSink::finish` with no parser primitive
file-line closure.

## Findings

### CH6-001 - Save-only symbolication is explicitly non-closing

Status: ACCEPT.

V3 preserves the PASS-1 samply-discipline limitation instead of papering it
over. P1-A states the retained parse capture is still
`--save-only --unstable-presymbolicate`, that saved profiles report
`symbolicated=false`, and that this is not a clean interactive samply pass
(`p1a-samply-mode-1.md:10`, `:47-53`, `:80`, `:136-137`). P1-B and P1-C keep
the same save-only command surface for direct and mode III
(`p1b-samply-mode-2.md:10`, `:43-45`, `:112-115`;
`p1c-samply-mode-3.md:10`, `:40-43`).

The V3 ledger makes the downgrade canonical: all rows are
`profile_signal_not_gate_admission`, and parse rows are not promoted beyond
their stated precision (`support/evidence-ledger-v3.md:23`, `:52-61`). Profile
provenance likewise retains V1 parse/typed as auditable capture artefacts with
binary hashes, not as a fully rebuildable command surface
(`support/profile-provenance-v3.md:49-52`).

### CH6-002 - Function-only sidecars are no longer treated as primitive closure

Status: ACCEPT.

Mode III moved from absent to captured, but V3 does not close the file-line
defect. P1-C says rank-1 rows remain `function-only-sidecar` unless a separate
source anchor is stated, and it keeps NEON file-line gaps as a CH6 risk
(`p1c-samply-mode-3.md:13-16`, `:63-67`, `:126-133`). The evidence ledger
states that all 85 mode-III rank-1 rows are citable by function and profile path
but remain `function-only-sidecar` for CH1/CH6 file-line closure
(`support/evidence-ledger-v3.md:78-91`).

P1-E also makes the synthesis boundary explicit: sidecar symbol resolution gives
file-line data for most Rust leaves, but ASM and system leaves remain
function-only and are "not silently resolved" (`p1e-hot-leaf-attribution.md:109-115`).
That is the V3 behavior CH6 asked for.

### CH6-003 - CSS is profiled, but parser hot-leaf closure is rejected

Status: ACCEPT.

CSS declaration-values is no longer missing all profile evidence, but V3 does
not use that fact as parser attribution. The evidence ledger classifies the row
as `css-profiled-nonparser-overhead`: equality, throughput, and a profile exist,
but the top leaves are timer/fact-sink overhead and the parser hot leaf is
unresolved (`support/evidence-ledger-v3.md:16-17`, `:100-104`).

P1-E and P1-F repeat the same non-closing boundary. P1-E names the top leaves as
17.6% `mach_absolute_time`, 13.7% `LocalFactSink::finish`, and 7.5%
`FactSink::finish`, and says longer or narrower profiling is needed before a
CSS parser hot leaf can be named (`p1e-hot-leaf-attribution.md:79-83`,
`:107-108`). P1-F marks CSS throughput as method-mismatched against the SK-V12
Criterion close and uses it as hot-leaf/equality signal, not demotion or
admission (`p1f-results-delta.md:79-88`, `:98-100`, `:122-125`).

### CH6-004 - Typed 7/17 is kept as a product-surface gap

Status: ACCEPT.

V3 stops counting the missing typed rows as profiled. P1-B says typed evidence
remains the V1 seven-row generated subset and that V2 did not invent rows for
the ten unsupported corpora (`p1b-samply-mode-2.md:92-95`, `:125-126`). P1-D
keeps typed PMU coverage to seven generated rows and calls the ten absent rows
product-surface gaps (`p1d-pmu-cycles.md:61-67`, `:129-130`). P1-F states that
the ten typed rows "cannot be counted as profiled or admitted"
(`p1f-results-delta.md:102-111`, `:120-121`).

The canonical ledger backs that up: seven typed rows are `json-typed-only`; the
remaining ten corpora are `missing-product-surface`, defined as absent from the
generated product surface and not profiled
(`support/evidence-ledger-v3.md:16`, `:19`, `:63-76`).

### CH6-005 - The gate boundary is explicit enough

Status: ACCEPT.

Some V3 text still uses ordinary measurement language such as "measured" or
"profile exists." In context, that is not a paper close. P1-F states its
classification is `profile_signal_not_gate_admission`, that later gate/REDRESS
waves alone can admit or demote rows, and that V2 profile signals do not create
implementation authority (`p1f-results-delta.md:13-16`, `:35-37`, `:124-131`).
The support ledger repeats the same boundary for every row
(`support/evidence-ledger-v3.md:23`).

CH6 therefore accepts the remaining measurement words as artefact-existence
claims, not closure claims.

## Disposition Matrix

| Artefact | CH6 disposition | Reason |
|---|---|---|
| P1-A | ACCEPT | Save-only parse evidence is retained with `symbolicated=false` and CH6 risk stated; V3 ledger downgrades parse envelopes/function-only rows. |
| P1-B | ACCEPT | Direct V2 is non-panic and citable, while save-only, timer noise, and typed 7/17 limits are explicit. |
| P1-C | ACCEPT | Mode-III coverage exists, but rank-1 file-line gaps are explicitly `function-only-sidecar` and non-closing. |
| P1-D | ACCEPT | Counter gaps are routed as unavailable, not zero or hidden. |
| P1-E | ACCEPT | Synthesis points to the ledger for resolved/unresolved authority and names CSS/function-only limits. |
| P1-F | ACCEPT | Row inventory keeps profile signals separate from gate admission and says missing typed rows are not profiled/admitted. |

## Required Follow-Up

No V4 CH6 fold is required for anti-paper-close. Future work remains if the
program wants stronger evidence than V3 currently has:

1. Recapture or post-symbolicate save-only profiles if interactive-quality
   symbol resolution is needed.
2. Re-extract mode-III rank-1 rows with per-row file:line closure before using
   them as precise primitive attributions.
3. Reprofile CSS narrowly enough to separate parser work from timer/fact-sink
   overhead.
4. Generate or route the ten absent typed product surfaces before counting them
   as typed profiles.

Those are real technical gaps, but V3 now labels them as non-closing. Overall
S-P1 V3 CH6 disposition: ACCEPT.
