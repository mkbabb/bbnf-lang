# PROVE-14 — ADVERSARIAL VERIFICATION (TAPE-IN-TS, F4)

**Verdict: CONFIRMED.** The claim reproduces on all three components: byte-exact
layout parity PROVEN, stop-early clears the 2x gate at ~4.4x (MET), full-walk at
parity (~0.91x, honestly UNMET). Magnitudes run modestly below the report but no
gate outcome changes. No edict violations found.

**Tree** prover worktree `wf_386900ed-462-14` @ `d2b32f6e3` (present, built artifact
`target/release/tape_dump` intact). **Runtime** node v26.0.0, aarch64, 18-core box.
Fresh dump regenerated; numbers reproduced.

## Fresh numbers (re-measured, N=40, 2 independent windows, cold per-parse)

Loadavg ~8 during my runs (report ran at ~18-20 — a LIGHTER regime, so absolutes
differ, but paired ratios are load-robust by design).

| workload | window | paired json/tape | 95% CI | median x | N | loadavg |
|---|---|--:|---|--:|--:|---|
| full walk | A | 0.915x | [0.876, 0.954] | 0.892 | 40 | 8.08/8.55/10.65 |
| full walk | B | 0.900x | [0.867, 0.932] | 0.893 | 40 | 7.99/8.52/10.61 |
| stop-early | A | 4.564x | [4.260, 4.868] | 4.442 | 40 | 7.99/8.52/10.63 |
| stop-early | B | 4.374x | [4.076, 4.671] | 4.248 | 40 | 8.23/8.55/10.60 |

Report claimed: full-walk **0.981x** [0.880, 1.082]; stop-early **4.782x** [4.199,
5.366]. My tape lane came in ~7-9% slower in both directions (0.98→0.91 full;
4.78→4.44 stop-early) — the tape is consistently a touch worse than reported, i.e.
the report did NOT flatter the tape. Every conclusion is unchanged: full-walk is
parity (not 2x), stop-early clears 2x by ~2.2x margin.

## Parity oracle (re-run, byte-exact)

`verify.mjs` on a fresh dump: all 6 structural counts match (objects 1264, arrays
1050, strings 18099, numbers 2109, bools 2791, nulls 1946), codeUnits 304329,
**fp 986912547** identical tape vs JSON.parse; partial get **fp 3431570937**,
codeUnits 170/170 PASS; payload arena empty, source/flag planes paired. Across all 4
bench runs, digest parity PASS over 320 cold children. The parity claim holds exactly
as reported.

## Attack list (what I tried to break; all survived)

1. **Opt-profile asymmetry between arms** — IMPOSSIBLE here: both arms are JS in fresh
   node children; no cargo profile distinguishes them. Only the dump bin is `--release`
   and it is not on either timed path.
2. **Warm contamination** — REFUTED: each sample is a fresh cold `node` child running
   exactly one timed op; `readRaw` setup is untimed and identical for both lanes.
3. **Tape-favoring accounting** — the tape lane's timed region *includes* `decodeTape`
   (the 118KB offset-plane typed-array copy). Counted against the tape; it still wins
   4.4x on stop-early and still loses on full-walk. Honest.
4. **Unfair stop-early comparison** — JSON.parse must parse the whole 631KB doc to hand
   back any field; the tape navigates via `nextSibling` depth-scan and materializes only
   3 leaves. This IS the On-Demand thesis, framed as such; both fold identical values →
   identical partial digest (parity PASS). Fair.
5. **Cherry-picked window** — 2 independent windows agree within ~4% per lane; the
   report's less-favorable result (full-walk) is honestly declared UNMET, not buried.
6. **x86 / SIMD tokens** — NONE (grep hits on "sse" were the substring inside "asserts").
   Pure scalar JS + arm64 build.
7. **Grammar-name / corpus gate** — `classify()` switches on the raw source byte, not any
   grammar name. The `statuses[0]/id_str/text/screen_name` fields in `pointer.mjs` are the
   workload definition applied SYMMETRICALLY to both lanes, not a parser gate.
8. **Monomorphism claim** — the 4 `TapeDoc` fields are assigned only in the constructor
   (grep confirms zero post-ctor mutation); single hidden class, allocation-free integer-
   cursor walk. Claim holds.
9. **Dump bin touching runtime paths** — REFUTED: `tape_dump.rs` calls the production
   `runtime::generated_json::parse` and reads only the public `Tape::{offsets,
   flag_cursors, flag_values, source, payloads}` accessors. `git diff --stat`: 7 new
   files, 703 insertions, ZERO runtime-source edits, no regen.

## Edict audit

- **aarch64-only**: PASS (no x86 tokens; arm64 build; node aarch64).
- **no name-gates**: PASS (byte-classify, symmetric workload fields).
- **parsimony**: PASS (703 LOC, 7 files, no runtime change, no regen, bin autodiscovered).
- **no stubs/workarounds**: PASS (real working decoder; parity proves correctness;
  `unescapeJson` handles the full escape set incl. `\u`).

## Verdict rationale

CONFIRMED, not CONFIRMED_DELTA: the ~7-9% magnitude softening (tape slightly slower
than reported, under a lighter load regime) is within run/load variance and flips NO
gate — full-walk stays sub-1.0 parity (2x UNMET, as the report itself concedes),
stop-early stays ~4.4x (2x MET), parity stays byte-exact. The report's PARTIAL verdict
(parity proven; stop-early MET; full-walk 2x honestly UNMET) is faithful to what
re-measurement shows.
