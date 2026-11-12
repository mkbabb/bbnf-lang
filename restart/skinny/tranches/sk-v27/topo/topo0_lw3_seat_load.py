#!/usr/bin/env python3
"""LW3 seat-load table (LW3-ORDER amendment 3, from the LW2 seat receipts).

The LW2 seats' receipts are their guarded runs. The guard's attribution log
(`~/.claude/guard/log/zone-runs.tsv`: run id, mode, start, end, ..., cwd, cmd)
names each run's working directory, and every LW2 seat worked in a worktree or
log directory named for it (`lw2-<seat>` or `lw2/<seat>`). This counts the
heavy runs (mode `heavy`; light and browser runs excluded) per LW2 seat, then
gives each LW3 seat its expected heavy-run count as the sum over the LW2 seats
whose work is its nearest analogue. A run whose cwd and cmd name no seat (for
example a shared script run from the primary checkout) is not attributed, so
every count is a lower bound; the unattributed LW2-window total is printed.

usage: topo0_lw3_seat_load.py <out.tsv>"""
import collections, pathlib, re, sys, time

LOG = pathlib.Path.home() / ".claude/guard/log/zone-runs.tsv"
out = pathlib.Path(sys.argv[1])
# The LW2 window: dispatch of LW2 (2026-11-09 00:00 local) to the LG-2 r8 close.
T0 = time.mktime(time.strptime("2026-11-09 00:00", "%Y-%m-%d %H:%M"))
T1 = time.mktime(time.strptime("2026-11-11 21:30", "%Y-%m-%d %H:%M"))
TOK = re.compile(r"lw2[-/]([a-z0-9][a-z0-9.-]*)")

lw2, unattributed = collections.Counter(), 0
for line in LOG.read_text(errors="replace").splitlines()[1:]:
    f = line.split("\t")
    if len(f) < 14 or f[1] != "heavy":
        continue
    try:
        start = float(f[2])
    except ValueError:
        continue
    m = TOK.search(f[12] + " " + f[13])
    if m:
        seat = re.sub(r"-(read|base)$", "", m.group(1))
        lw2[seat] += 1
    elif T0 <= start <= T1:
        unattributed += 1

# LW3 seat -> (its LW2 analogues, why). Letter-suffixed dispatches (S3b, S4b,
# S7b..d, ...) share their seat's row: the analogue is per seat, not per dispatch.
LW3 = [
    ("S1 ENTRY", ["s1-custody"], "entry/custody; S1 in LW3 reads only (0 cargo builds planned)"),
    ("S2 CI", ["s4-toolchain"], "CI and workflow work; most reads are CI runs, not local"),
    ("S3 PP (+S3b, S21)", ["s8-pp"], "pprint lane"),
    ("S4 RX (+S4b, S4c)", ["s7-rx", "s4b-regex"], "regex lane, read worktrees included"),
    ("S5 ORACLE", ["s5-comp"], "BL test-crate work"),
    ("S6 KERNEL", ["s3-dag"], "PT crate move with law suites"),
    ("S7 SPINE (+S7b..d)", ["s1-custody", "s3-dag"], "the merge and its full workspace reads"),
    ("S8 SEM (+S8b)", ["s5-comp"], "compiler semantics on BL"),
    ("S9 PERF (+S9b..d)", ["s6-perf"], "SIMD tiers, Miri, guard-page, regen"),
    ("S10 GEN (+S10b)", ["s2-census"], "lints and law tests over the workspace"),
    ("S11 USE", ["s10-use"], "consumer reads (published artifacts)"),
    ("S12 CON-SPEC", ["s9-con-spec"], "contest arms and witnesses"),
    ("S13 MIG", ["s7-rx"], "every grammar parsed at two commits"),
    ("S14 BENCH", ["s11-bench"], "the bench window (exclusive)"),
    ("S15 CRITIC-A", ["s12-critic"], "review; reads only"),
    ("S16 CRITIC-B", ["s13-critic"], "review; reads only"),
    ("S17 ADJ", ["s14-adjudicator"], "no product code"),
    ("S18 FREEZE", ["s15-freeze"], "digest tests"),
    ("S19 PINS", ["topo-4b", "topo-4d", "topo-4e", "topo-4f"], "re-pin and semver probes"),
    ("S20 GATE (per reading)", ["r8", "r8-verify"], "one full LG reading (r8 was the TAKEN one)"),
]
rows = ["kind\tseat\tlw2_analogues\theavy_runs\tbasis"]
for seat, n in sorted(lw2.items(), key=lambda x: (-x[1], x[0])):
    rows.append(f"lw2\t{seat}\t-\t{n}\tguard attribution (cwd or cmd names the seat)")
rows.append(f"lw2\t(unattributed, LW2 window)\t-\t{unattributed}\theavy runs machine-wide 2026-11-09 00:00 to 11-11 21:30 naming no lw2 seat (other programmes included)")
for seat, an, why in LW3:
    keys = {k for k in lw2 for a in an if k == a or k.startswith(a + "-")}  # each LW2 seat counted once
    n = sum(lw2[k] for k in keys)
    rows.append(f"lw3\t{seat}\t{','.join(an)}\t{n}\t{why}; lower bound")
out.write_text("\n".join(rows) + "\n")
print("\n".join(rows))
