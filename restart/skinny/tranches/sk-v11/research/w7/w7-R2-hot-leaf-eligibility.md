# SK-V11 W7 R2 - Hot-Leaf Eligibility

Pass: W7 Phase 1 Research.
Agent: R2 fresh profile and hot-leaf eligibility.
Date: 2026-05-20.
Scope: determine whether W7 can enter planning through
`G-W7-DIGEST-SINK`.
Output: this read-only artifact.

## Verdict

W7 should block before plan.

There is no fresh post-W6 profile capture under `/tmp` that independently names
`output_digest_hash` after REDRESS 117. The available behavior profile is still
the SK-V11-open S-P1 capture at `/tmp/skv11-p1`: W3/W4 rejected their source
patches, W5 and W6 blocked before source redress, and W6 records no behavior
source, generated runtime, SIMD, benchmark body, gate/report schema, or
`RESULTS.md` movement. A local source-diff check from profile SHA `3ce75df4`
to current HEAD shows only W1a gate/report edits in the W7-adjacent owner set;
`direct_struct.rs`, generated JSON runtime, parse-that-regex, and SIMD hot-path
sources are unchanged.

That makes S-P1 behavior-equivalent evidence, not the literal fresh post-W6
profile required by SPEC Section 11 and P3-C. Under the existing S-P1 evidence,
`output_digest_hash` is a guard-row or weak support leaf, not a bounded residual
row mover with plausible floor closure.

## Required Gate

SPEC Section 11 limits W7 to C8 output digest/hash oracle or per-product host
sink only. Its entry gate requires W3-W6 dispositions plus CHALLENGE acceptance
that output digest/hash is an observed limiting hot leaf for a bounded selected
product-row subset, with exact scalar fold/mix source, output plane, and
independent oracle named (`SPEC.md:648`-`666`).

`G-W7-DIGEST-SINK` then requires selected direct rows whose fresh post-W6
profile still names `output_digest_hash` as limiting to meet the Section 0.4 floors on
Track 1 and Track 2/oracle, or a selected non-JSON host sink to improve at
least 1.0% with strict output equality. Digest/hash state may not enter generic
parser crates, cache hints cannot admit the wave, and guard floors must hold
(`SPEC.md:674`-`687`; `p3c-falsifiability-gates.md:85`).

REDRESS 117 carries forward the blocking edges: REDRESS 54/55/66/69, 64, 82,
107, 108, 113, 116, and 117. The W6 decoded-byte fold was rejected because it
used the same sink seam, current `JsonDirectDigest` length/fingerprint contract,
and allocation-removal claim as the REDRESS 54 sink-local decoded stats/hash
family (`REDRESS.md:3434`-`3460`; `HANDOFF.md:119`-`129`). W7 cannot re-enter
through that seam under a new name.

## Profile Freshness

The only profile root available is `/tmp/skv11-p1`, accepted by S-P1 hardening:
34 parse, 34 direct, and 14 typed PMU rows with full corpus coverage. P1-E
states the direct xctrace summary has 48 product-plane traces, covering 17
direct Track 1/Track 2 rows plus 7 typed Track 1/Track 2 rows
(`p1e-hot-leaf-attribution.md:76`-`89`). S-P1 converged with `output_digest_hash`
accepted only as one hot-family antecedent, not as row admission evidence
(`HARDENING-S-P1-CONVERGED.md:22`-`55`).

Post-W6 behavior freshness is inferential only:

- W6 REDRESS 117 says no source patch was attempted and no behavior/runtime/
  SIMD/benchmark/RESULTS row moved.
- W5 similarly blocked before source redress.
- W3/W4 rejected source routes and saved rejected patches.
- `git diff --stat 3ce75df4..HEAD` over W7 hot-path owners shows only
  `skinny/crates/bbnf-bench/src/bin/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs` changed. Those are not the direct
  digest hot path.

Therefore a W7 plan cannot truthfully say "fresh post-W6 profile" exists. It
can only say "the S-P1 profile is still behavior-equivalent because no relevant
hot-path source landed." That is weaker than the gate wording and must go to
BLOCKED unless the orchestrator first authorizes a new profile capture.

## Row Eligibility

Direct floors are the SPEC Section 0.4 floors (`SPEC.md:117`-`135`). P1-B/P1-E name
the relevant hot leaves.

| Row | Current T1/T2 vs floor | Digest/hash evidence | Eligibility |
|---|---:|---|---|
| `apache_builds/direct_to_struct` | 11254 / 10189 vs guard floors 11028 / 9996 | P1-E direct guard: wrapping-add digest 18.4% T1 and 15.2% T2 (`p1e:201`-`203`; `p1b:216`-`226`). | Guard only. It is already `A / GO`, not a residual row W7 can admit. Use as regression sentinel if any W7 route existed. |
| `distinct_values/direct_to_struct` | 1750 / 1625 vs floor 2658 | P1-E residual: T1 fold string 11.6%, T2 option-copied support 9.0% (`p1e:153`-`164`). | Weak profile candidate only. It is the strongest residual digest signal, but the row needs +908 Mbps T1 and +1033 Mbps T2. Removing every visible digest/support cost cannot plausibly close both tracks while tiny-string and whitespace remain larger limits. |
| `random/direct_to_struct` | 7693 / 6949 vs floor 7878 | P1-E residual: T1 option-copied support 6.6%, T2 wrapping-add digest 8.5% (`p1e:153`-`164`; `p1b:178`). | Not eligible as a selected W7 target. T1 is near floor but not digest-limited; T2 needs +929 Mbps and the digest leaf is below the gap. |
| `update_center/direct_to_struct` | 8187 / 7474 vs floor 10059 | P1-E residual: T1 wrapping-add digest 7.9%, but Track 2 is tiny/string/movemask dominated (`p1e:153`-`164`; `p1b:176`). | Not eligible. The floor gap is too large and digest/hash is not limiting across both tracks. |
| `unicode_mixed/direct_to_struct` | 3753 / 2427 vs floor 2588 | P1-E W0-clamped row: full string, unescape, and validate escape dominate both tracks (`p1e:184`-`189`). | Not W7 eligible. This was W6's row, and REDRESS 117 blocks the decoded-byte sink fold route. |
| `unicode_escapes/direct_to_struct` | 1345 / 1341 vs floor 3441 | P1-E residual: unescape/full-string/hex dominate (`p1e:162`; `p1b:183`). | Not W7 eligible. Digest/hash is not limiting. |
| `y_string_unicode/direct_to_struct` | 1983 / 1029 vs floor 3950 | P1-E residual: hex decode and unescape dominate (`p1e:164`; `p1b:185`). | Not W7 eligible. Digest/hash is not limiting. |
| `twitter`, `github_events`, `canada`, `mesh`, `gsoc-2018`, `instruments`, `numbers` | See SPEC §0.4 floors | Their P1-E limiting leaves are tiny/string/whitespace/movemask, number/sequence, or W0-clamped non-admission causes, not output digest/hash (`p1e:153`-`188`). | Not W7 eligible on current evidence. |

The only residual row with a meaningful `output_digest_hash` profile antecedent
is `distinct_values`, and even that is not a plan-quality candidate because the
visible digest work is smaller than the required floor delta and is not the top
Track 2 limiter. `random` and `update_center` are support-only scouts, not
entry-gate rows. `apache_builds` is the strongest digest row, but it is an
already-admitted guard.

## Entry-Gate Decision

W7 lacks an admissible entry-gate candidate now.

The phase should not proceed to a W7 plan unless one of these happens first:

1. A true post-W6 product-plane profile is captured and names
   `output_digest_hash` as a limiting leaf on a bounded residual subset in both
   Track 1 and Track 2/oracle.
2. That subset has plausible floor math: the measured digest/hash self-time must
   be large enough to cover the required Mbps gaps without relying on parser
   semantics, semantic hash side tables, cache hints, or the REDRESS 54 decoded
   stats seam.
3. The plan can name an independent oracle and scalar fold/mix source that does
   not couple Track 2 to Track 1 and does not move digest/hash state into generic
   parser crates.

Absent those facts, W7 should record `BLOCKED before plan` and route the direct
residual rows to W8 fixpoint accounting rather than spending a CHALLENGE cycle
on a host-sink route the current profile does not support.
