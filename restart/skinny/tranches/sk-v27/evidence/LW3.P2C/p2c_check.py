#!/usr/bin/env python3
"""LW3 Phase 2 checkpoint record units (seat S7 SPINE).

Usage: p2c_check.py <unit> <rev>
  unit: rulings | r02 | orc2ws | perf1ag
  rev:  the bbnf-lang commit read (files are read with `git show <rev>:<path>`).
Exit 0 = GREEN, 1 = RED (each reason printed), 2 = usage.

rulings  RULINGS.md carries `## Addendum, 2026-11-12c`, one row per ask, each
         ruled row marked "delegate, owner may overturn" and holding its
         named facts; DP-3 is recorded as unanswered; the sealed text before
         the addendum is unchanged (append-only).
r02      LEDGER.md carries one status row closing LW3.R0-2 (GREEN), citing
         its own nextest, clippy and regen reads and their logs.
orc2ws   LEDGER.md carries one PLANNED row for LW3.ORC2-WS-C naming its seat
         S5c ORC-WS, Phase 3b1, its gate and the line LG-3.1 was read at.
perf1ag  LEDGER.md carries one GREEN row for LW3.PERF-1a-G with its RED,
         GREEN and mutant reads and the parse-that merge.
"""
import re, subprocess, sys

SK = "restart/skinny/tranches/sk-v27"
BASE = "28eeed1c42f94986f9baa6b85966a3cd0ac10027"  # sk-v27-lw3 at Phase 3a entry


def git(*a, check=True):
    p = subprocess.run(["git", *a], capture_output=True, text=True)
    if check and p.returncode != 0:
        raise RuntimeError(f"git {' '.join(a)}: {p.stderr.strip()}")
    return p


def show(rev, path):
    p = git("show", f"{rev}:{path}", check=False)
    return p.stdout if p.returncode == 0 else ""


MARK = "delegate, owner may overturn"


def check_rulings(rev):
    bad = []
    t = show(rev, f"{SK}/RULINGS.md")
    old = show(BASE, f"{SK}/RULINGS.md")
    if not t.startswith(old.rstrip("\n")):
        bad.append("RULINGS.md: text before the addendum changed (not append-only)")
    m = re.search(r"^## Addendum, 2026-11-12c\b.*?(?=^## |\Z)", t, re.M | re.S)
    if not m:
        return bad + ["RULINGS.md has no `## Addendum, 2026-11-12c` section"]
    sec = m.group(0)
    if MARK not in sec.split("\n| # |")[0]:
        bad.append("2026-11-12c: the preamble does not say every ruled row is a delegate ruling")
    want = {
        "P2C-1": ["LW3.PERF-1a-G", "lw3/s9-perf", "rust/simd/benches/stage1_split.rs:4",
                  "git grep -niE bbnf -- rust/simd", "cfe77fb", "--no-ff",
                  "before COMP-1b pins parse-that"],
        "P2C-2": ["LW3.COMP-1b-W", "first substantive bbnf-lang unit",
                  "born RED on a ROUTE to LW5.COMP-1-MINE", "before COMP-1b"],
        "P2C-3": ["LW3.R0-2", "nextest", "clippy", "regen", "lane's base"],
        "PP2-G3": ["cured in LW3, not carried", "fourth clause",
                   "or cures a defect that a custody-corpus gate of LW3 names",
                   "S3c-PP", "Phase 3b1", "g3_trivia.rs", "un-narrowed"],
        "PP2-C": ["seat's reading stands, provisionally"],
        "PP2-B": ["seat's reading stands, provisionally"],
        "PP1-A": ["seat's reading stands, provisionally"],
        "PP1-B": ["seat's reading stands, provisionally"],
        "S9-QW": ["Phase 5", "G-K1-S1-NEON", "G-K1-S1-FUSED", "before PERF-1b"],
        "ORC2-WS": ["LW3-ORDER.md:224", "per grammar per arm", "root",
                    "QX-1's third clause", "LW3.ORC2-WS-C", "S5c ORC-WS", "Phase 3b1",
                    "rb_root_whitespace.rs", "g1.rs:220-223"],
        "QX-1 (restated)": ["(1)", "(2)", "(3)", "(4)", "J-14..J-16",
                            "custody-corpus gate of LW3", "regen --check"],
        "F2": ["3bdc88cfa", "deviation", "no history rewrite"],
    }
    for rid, needles in want.items():
        rows = [l for l in sec.split("\n") if l.startswith(f"| {rid} |")]
        if len(rows) != 1:
            bad.append(f"{rid}: {len(rows)} rows in 2026-11-12c (want 1)")
            continue
        if MARK not in rows[0]:
            bad.append(f"{rid}: row is not marked \"{MARK}\"")
        for n in needles:
            if n not in rows[0]:
                bad.append(f"{rid}: row lacks {n!r}")
    dp3 = [l for l in sec.split("\n") if l.startswith("| DP-3 |")]
    if len(dp3) != 1:
        bad.append(f"DP-3: {len(dp3)} rows (want 1)")
    elif not all(n in dp3[0] for n in ("UNANSWERED", "DECISION-PLAN.lw3.json", "R1", "owner")):
        bad.append("DP-3: row does not record the ask as unanswered and the owner's")
    return bad


STATUS = re.compile(r"\| (RED|GREEN|PLANNED|RULING|OPEN|CARRIED|RECORDED|CLOSED)[^|]*\|\s*$")


def one_row(rev, title, status, needles):
    led = show(rev, f"{SK}/LEDGER.md")
    old = show(BASE, f"{SK}/LEDGER.md")
    bad = []
    if not led.startswith(old.rstrip("\n")):
        bad.append("LEDGER.md: sealed text changed (not append-only)")
    hit = [l for l in led.split("\n") if l.startswith("| 2026-") and f"**{title}" in l]
    if len(hit) != 1:
        return bad + [f"{title}: {len(hit)} LEDGER rows (want 1)"]
    m = STATUS.search(hit[0])
    if not m:
        bad.append(f"{title}: row has no status cell")
    elif m.group(1) != status:
        bad.append(f"{title}: status {m.group(1)} (want {status})")
    for n in needles:
        if n not in hit[0]:
            bad.append(f"{title}: row lacks {n!r}")
    return bad


def check_r02(rev):
    return one_row(rev, "Status addendum for `LW3.R0-2`", "GREEN",
                   ["83f1fd037", BASE[:9], "nextest", "clippy", "regen --check",
                    "cargo clean -p", "s7-spine/r02-", "P2C-3"])


def check_orc2ws(rev):
    return one_row(rev, "LW3.ORC2-WS-C", "PLANNED",
                   ["S5c ORC-WS", "Phase 3b1", "rb_root_whitespace.rs", "J-14..J-16",
                    "LW3-ORDER.md:224", "QX-1", "regen --check", "ruled.tsv"])


def check_perf1ag(rev):
    return one_row(rev, "LW3.PERF-1a-G", "GREEN",
                   ["cfe77fb", "stage1_split.rs:4", "RED ×2", "GREEN ×2", "Mutant",
                    "lw3/s9-perf", "--no-ff", "sk-v27-lw3", "P2C-1"])


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 2
    unit, rev = sys.argv[1], sys.argv[2]
    fn = {"rulings": check_rulings, "r02": check_r02, "orc2ws": check_orc2ws,
          "perf1ag": check_perf1ag}.get(unit)
    if not fn:
        print(__doc__)
        return 2
    full = git("rev-parse", rev).stdout.strip()
    bad = fn(full)
    print(f"unit={unit} rev={full} reasons={len(bad)}")
    for b in bad:
        print("RED", b)
    print("GREEN" if not bad else "RED")
    return 0 if not bad else 1


if __name__ == "__main__":
    sys.exit(main())
