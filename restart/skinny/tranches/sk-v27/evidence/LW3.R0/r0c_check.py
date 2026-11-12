#!/usr/bin/env python3
"""LW3 R0 critic rows (seat S3b-PP, the first BL-writing seat of its pair).

Usage: r0c_check.py <rev> [--plan-file FILE]
  Reads PLAN.md and the cited passages with `git show <rev>:<path>`.
  --plan-file reads PLAN.md from FILE instead (the mutant read).
Exit 0 = GREEN, 1 = RED (each reason printed), 2 = usage.

GREEN iff PLAN.md carries section 7.4 with rows G1-P23..G1-P27, each once,
each amending text citing DIG-G1, each cited passage still holding its
quoted sealed text at the cited line, and PLAN.md up to the end of 7.3
byte-equal to the sealed base (append-only).
"""
import re, subprocess, sys

SK = "restart/skinny/tranches/sk-v27"
BASE = "d98914e34"

ROWS = {
    "G1-P23": [("designs/programme.md", 483, "It is deleted. ASM earns its place or goes.")],
    "G1-P24": [("designs/pprint.md", 206, "With no gain, the kernel is deleted."),
               ("designs/pprint.md", 207, "or deletion")],
    "G1-P25": [("designs/compiler.md", 114, "A kernel missing any of the four is deleted"),
               ("PLAN.md", 59, "measured win on its runner or it is deleted")],
    "G1-P26": [("PLAN.md", 56, "re-admitted only as passes that win a trial (C2-5: a root trial can only *delete*)")],
    "G1-P27": [("LW3-ORDER.md", 107, "deletion is LW7"),
               ("LW3-ORDER.md", 290, "CB-13 judges them there"),
               ("LW3-ORDER.md", 299, "would delete the organ"),
               ("LW3-ORDER.md", 300, "losers deleted"),
               ("LW2-ORDER.md", 88, "goes to the simpler organ")],
}


def show(rev, path):
    p = subprocess.run(["git", "show", f"{rev}:{path}"], capture_output=True, text=True)
    return p.stdout if p.returncode == 0 else None


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2
    rev = argv[1]
    plan = show(rev, f"{SK}/PLAN.md")
    if "--plan-file" in argv:
        plan = open(argv[argv.index("--plan-file") + 1]).read()
    bad = []
    base = show(BASE, f"{SK}/PLAN.md")
    if plan is None or base is None:
        print("RED: PLAN.md unreadable")
        return 1
    if not plan.startswith(base.rstrip("\n")):
        bad.append("PLAN.md is not the sealed base plus appended text (not append-only)")
    m = re.search(r"^### 7\.4 .*?(?=^### |^## |\Z)", plan, re.M | re.S)
    if not m:
        bad.append("PLAN.md has no section 7.4 (the R0 critic's DIG-G1 rows)")
        sec = ""
    else:
        sec = m.group(0)
    for rid, cites in ROWS.items():
        rows = [l for l in sec.split("\n") if l.startswith(f"| {rid} |")]
        if len(rows) != 1:
            bad.append(f"{rid}: {len(rows)} rows in 7.4 (want 1)")
            continue
        cells = [c.strip() for c in rows[0].strip().strip("|").split("|")]
        if len(cells) < 4 or not cells[3].startswith("DIG-G1"):
            bad.append(f"{rid}: amending cell does not cite DIG-G1")
        for path, line, frag in cites:
            src = show(rev, f"{SK}/{path}")
            if src is None:
                bad.append(f"{rid}: {path} absent at {rev}")
                continue
            lines = src.split("\n")
            if line > len(lines) or frag not in lines[line - 1]:
                bad.append(f"{rid}: {path}:{line} does not hold \"{frag}\"")
            if path.split("/")[-1].split(".")[0] not in rows[0] and path not in rows[0]:
                bad.append(f"{rid}: row does not cite {path}")
    if bad:
        for b in bad:
            print("RED:", b)
        return 1
    print(f"GREEN: 7.4 carries G1-P23..G1-P27 at {rev}; every cited passage holds its quote; PLAN.md append-only")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
