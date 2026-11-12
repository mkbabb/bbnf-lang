#!/usr/bin/env python3
"""LW3 R0-FOLD falsifiers (documents and custody only).

Usage: r0_check.py <unit> <rev> [--worktree DIR]
  unit: rulings | fold | status | tree | cust
  rev:  the commit read (files are read with `git show <rev>:<path>`, never
        from the working tree, so a read names exactly one tree).
Exit 0 = GREEN, 1 = RED (each reason printed), 2 = usage.
"""
import csv, hashlib, io, os, re, subprocess, sys

SK = "restart/skinny/tranches/sk-v27"
EV = f"{SK}/evidence/LW3.R0"
DIG_BRANCH = "origin/sk-v27-archive-dig"
DIG_TIP = "7fc26d0d7631c8631f33be4e93d623e9ae9eb142"


def git(*a, check=True):
    p = subprocess.run(["git", *a], capture_output=True, text=True)
    if check and p.returncode != 0:
        raise RuntimeError(f"git {' '.join(a)}: {p.stderr.strip()}")
    return p


def show(rev, path):
    p = git("show", f"{rev}:{path}", check=False)
    return p.stdout if p.returncode == 0 else None


def ledger_rows(text):
    return [l for l in text.split("\n") if l.startswith("| 2026-")]


# ---------------------------------------------------------------- rulings
RULING_IDS = ["ORC-1", "ORC-2", "RX-1d-Q1", "PRG-1", "SIMD-V", "IAI-A",
              "LED2-A", "LG33-A", "S8-A", "TGT-1", "DIG", "DIG-CUST-1",
              "DIG-G1", "DIG-A9", "DIG-QW"]


def check_rulings(rev):
    bad = []
    t = show(rev, f"{SK}/RULINGS.md") or ""
    m = re.search(r"^## Addendum, 2026-11-12a\b.*?(?=^## |\Z)", t, re.M | re.S)
    if not m:
        return ["RULINGS.md has no `## Addendum, 2026-11-12a` section"]
    sec = m.group(0)
    for rid in RULING_IDS:
        rows = [l for l in sec.split("\n") if l.startswith(f"| {rid} |")]
        if len(rows) != 1:
            bad.append(f"{rid}: {len(rows)} rows in 2026-11-12a (want 1)")
        elif "delegate, owner may overturn" not in rows[0]:
            bad.append(f"{rid}: row is not marked \"delegate, owner may overturn\"")
    if "OWN-1" not in sec or "REPLACES" not in sec:
        bad.append("DIG-G1 does not say it REPLACES OWN-1")
    return bad


# ---------------------------------------------------------------- fold
def dig_ids(rev):
    a = show(rev, f"{SK}/AMENDMENTS-FROM-THE-DIG.md")
    if a is None:
        return None
    ids = set(re.findall(r"^\| (DIG-[A-Za-z0-9-]+) \|", a, re.M))
    ids |= {"DIG-O-" + x for x in re.findall(r"^\| O-(\d+) \|", a, re.M)}
    ids |= set(re.findall(r"\((DIG-CERT-0)\)", a))
    return ids


def appended_only(rev, path):
    """The file at rev begins with the file at the lane base, byte for byte
    up to the base's last line: sealed text is not edited, only appended to."""
    base = git("merge-base", rev, "origin/sk-v27-lw3").stdout.strip()
    old = show(base, path) or ""
    new = show(rev, path) or ""
    return new.startswith(old.rstrip("\n"))


def check_fold(rev):
    bad = []
    if git("merge-base", "--is-ancestor", DIG_TIP, rev, check=False).returncode != 0:
        bad.append(f"the archive dig {DIG_TIP[:9]} is not merged into {rev}")
    ids = dig_ids(rev)
    if ids is None:
        bad.append("AMENDMENTS-FROM-THE-DIG.md absent at rev")
        ids = set()
    led = show(rev, f"{SK}/LEDGER.md") or ""
    fold = [l for l in ledger_rows(led) if "(R0-FOLD, AMENDMENTS-FROM-THE-DIG.md" in l]
    for i in sorted(ids):
        hit = [l for l in fold if f"**{i} " in l]
        if len(hit) != 1:
            bad.append(f"{i}: {len(hit)} fold rows in LEDGER (want 1)")
            continue
        r = hit[0]
        if "admitted" not in r or "homed:" not in r:
            bad.append(f"{i}: fold row lacks `admitted` or `homed:`")
        if not re.search(r"\| (PLANNED|GREEN|RULING|OPEN|CARRIED)[^|]*\|\s*$", r):
            bad.append(f"{i}: fold row has no status cell")
    plan = show(rev, f"{SK}/PLAN.md") or ""
    m = re.search(r"^## 7\. Addenda.*", plan, re.M | re.S)
    padd = m.group(0) if m else ""
    if not padd:
        bad.append("PLAN.md has no `## 7. Addenda` section")
    # DIG-G1: each loser-deleting passage exists in the sealed text at rev and
    # has one PLAN §7 row that cites DIG-G1 and names file:line.
    pt = show(rev, f"{EV}/dig-g1-passages.tsv")
    if pt is None:
        bad.append("dig-g1-passages.tsv absent at rev")
    else:
        for r in csv.DictReader(io.StringIO(pt), delimiter="\t"):
            src = show(rev, r["file"]) or ""
            lines = src.split("\n")
            n = int(r["line"])
            if n > len(lines) or r["quote"] not in lines[n - 1]:
                bad.append(f"{r['id']}: passage not at {r['file']}:{n}")
            short = r["file"].replace(f"{SK}/", "")
            rows = [l for l in padd.split("\n")
                    if l.startswith(f"| {r['id']} |")]
            if len(rows) != 1:
                bad.append(f"{r['id']}: {len(rows)} PLAN §7 rows (want 1)")
            elif "DIG-G1" not in rows[0] or f"{short}:{n}" not in rows[0]:
                bad.append(f"{r['id']}: PLAN §7 row does not cite DIG-G1 and {short}:{n}")
    # The dig's grep (DIG-G1 falsifier): every hit at rev, in the sealed
    # sources, is one of the listed passages.
    listed = set()
    if pt:
        for r in csv.DictReader(io.StringIO(pt), delimiter="\t"):
            listed.add((r["file"], int(r["line"])))
    g = git("grep", "-nE",
            "simpler organ wins|keep/delete|trial losers|or the kernel is deleted|off by default or removed",
            rev, "--", f"{SK}/designs", f"{SK}/PLAN.md", f"{SK}/LW3-ORDER.md",
            "bench/PREREG.md", check=False).stdout
    for h in g.strip().split("\n"):
        if not h:
            continue
        _, f, ln, _ = h.split(":", 3)
        if (f, int(ln)) not in listed:
            bad.append(f"DIG-G1 grep hit {f}:{ln} is not an amended passage")
    # The plan units the dig names, and the 14 amendment-9 ids (DIG-G2).
    units = ["LW5.COMP-1-MINE", "LW5.COMP-1-BRIDGE", "LW5.COMP-5",
             "LW5.COMP-1-OPTIR", "LW4.TOPO-PIN", "LW4.COMP-5", "LW4.COMP-6",
             "LW4.PERF-3", "LW4.PERF-4", "LW5.COMP-1-PAT", "LW5.COMP-4",
             "LW5.COMP-6"]
    a9 = ["LW4.PERF-2", "LW4.COMP-4", "LW4.CON-2", "LW4.PERF-1-U5",
          "LW4.CON-1-J", "LW4.RX-2-BR", "LW4.RX-2-DOC", "LW4.PP-1-NAH",
          "LW4.PP-1-LL", "LW4.PP-3-TS", "LW5.COMP-1-OPTIR", "LW5.COMP-3",
          "LW5.PP-1-H", "LW7.CON-1-REQ"]
    for u in units + a9:
        if not re.search(r"^\| [^|]* \| " + re.escape(u) + r"\b", padd, re.M) \
                and f"`{u}`" not in padd:
            bad.append(f"{u}: no PLAN §7 row")
    if "BEFORE the normalizer" not in padd:
        bad.append("PLAN §7 does not home the e-graph BEFORE the normalizer")
    if "quiet certification window" not in padd:
        bad.append("PLAN §7 does not home the quiet certification window")
    for p in (f"{SK}/PLAN.md", f"{SK}/LEDGER.md", f"{SK}/RULINGS.md"):
        if not appended_only(rev, p):
            bad.append(f"{p}: sealed text changed (not append-only)")
    return bad


# ---------------------------------------------------------------- status
EIGHT = {"LW3.COMP-5b": "GREEN", "LW3.RX-0": "GREEN", "LW3.RX-1a": "GREEN",
         "LW3.RX-1b": "GREEN", "LW3.RX-1c": "GREEN", "LW3.RX-3": "GREEN",
         "LW3.COMP-3a": "RED", "LW3.RX-1d": "GREEN"}


def has_status_cell(row):
    cells = [c.strip() for c in row.strip().strip("|").split("|")]
    return len(cells) >= 3 and re.match(
        r"^(PLANNED|OPEN|RED|GREEN|CLOSED|CARRIED|UNREAD|RULING|RECORDED)\b", cells[-1])


def check_status(rev):
    bad = []
    rows = ledger_rows(show(rev, f"{SK}/LEDGER.md") or "")
    r01 = [r for r in rows if "**LW3.R0-1 " in r]
    if len(r01) != 1:
        return [f"LW3.R0-1 status row: {len(r01)} (want 1)"]
    r = r01[0]
    if not has_status_cell(r):
        bad.append("LW3.R0-1 row has no status cell")
    for u, st in EIGHT.items():
        own = [x for x in rows if x.split("|")[2].lstrip(" *").startswith(u + " ")]
        if len(own) != 1:
            bad.append(f"{u}: {len(own)} unit rows (want 1)")
        elif has_status_cell(own[0]):
            bad.append(f"{u}: its row already has a status cell (stale list)")
        if not re.search(r"`" + re.escape(u) + r"` → " + st + r"\b", r):
            bad.append(f"{u}: R0-1 does not give it {st}")
    for held, why in (("LW3.COMP-3a", "ORC-1"), ("LW3.COMP-3a", "ORC-2"),
                      ("LW3.RX-1d", "RX-1d-Q1")):
        if why not in r:
            bad.append(f"{held}: R0-1 does not name its hold {why}")
    return bad


# ---------------------------------------------------------------- tree
def check_tree(rev):
    rows = ledger_rows(show(rev, f"{SK}/LEDGER.md") or "")
    hit = [r for r in rows if "**LW3.R0-2 " in r]
    if len(hit) != 1:
        return [f"LW3.R0-2 merged-tree read row: {len(hit)} (want 1)"]
    r = hit[0]
    bad = []
    for need in ("83f1fd037", "1501/1501", "clippy", "regen",
                 "read once, in the merge message; re-read owed at the next BL GREEN"):
        if need not in r:
            bad.append(f"LW3.R0-2 row lacks `{need}`")
    if not has_status_cell(r):
        bad.append("LW3.R0-2 row has no status cell")
    return bad


# ---------------------------------------------------------------- cust
STOPGAP = os.path.expanduser("~/.dev-logs/bbnf-lockstep/archive-dig/custody-bounded-sum")
CUST = {  # custody ref -> (stopgap dir, base, {stopgap file: tracked path})
    "sk-lineage/bounded-sum-custody": ("wf_fce2ec2b-53a-32", "ce51c36fcbdf5108eb028005586672d9a6d2ca1b", {
        "src-constraint/bounded_sum.rs": "crates/csp-solver/src/constraint/bounded_sum.rs",
        "src-constraint/mod.rs": "crates/csp-solver/src/constraint/mod.rs",
        "tests/bounded_sum.rs": "crates/csp-solver/tests/bounded_sum.rs",
        "examples/scan_plan_solve.rs": "crates/csp-solver/examples/scan_plan_solve.rs"}),
    "sk-lineage/bounded-sum-custody-wf28": ("wf_63419230-36d-28", "d2b32f6e38b9fbc366efe19193a68ccc4980c9ac", {
        "src-constraint/bounded_sum.rs": "crates/csp-solver/src/constraint/bounded_sum.rs",
        "src-constraint/mod.rs": "crates/csp-solver/src/constraint/mod.rs",
        "tests/bounded_sum.rs": "crates/csp-solver/tests/bounded_sum.rs",
        "examples/scan_plan_budget.rs": "crates/csp-solver/examples/scan_plan_budget.rs"}),
    "sk-lineage/bounded-sum-custody-wf30": ("wf_63419230-36d-30", "d2b32f6e38b9fbc366efe19193a68ccc4980c9ac", {
        "src-constraint/bounded_sum.rs": "crates/csp-solver/src/constraint/bounded_sum.rs",
        "src-constraint/mod.rs": "crates/csp-solver/src/constraint/mod.rs",
        "tests/bounded_sum.rs": "crates/csp-solver/tests/bounded_sum.rs"}),
}


def check_cust(rev):
    bad = []
    log = git("log", "--remotes=origin", "--oneline", "--", "**/bounded_sum.rs", check=False).stdout.strip()
    if not log:
        bad.append("`git log --remotes=origin -- '**/bounded_sum.rs'` is empty")
    sums = {}
    for l in open(os.path.join(STOPGAP, "SHA256SUMS")):
        h, p = l.split(None, 1)
        sums[p.strip().lstrip("./")] = h
    for ref, (d, base, files) in CUST.items():
        rr = git("rev-parse", "--verify", "-q", f"origin/{ref}", check=False).stdout.strip()
        if not rr:
            bad.append(f"origin/{ref} absent")
            continue
        if git("merge-base", "--is-ancestor", base, rr, check=False).returncode != 0:
            bad.append(f"origin/{ref} does not descend from its base {base[:9]}")
        for sg, tp in files.items():
            want = sums.get(f"{d}/{sg}")
            blob = subprocess.run(["git", "show", f"{rr}:{tp}"], capture_output=True).stdout
            got = hashlib.sha256(blob).hexdigest()
            if want != got:
                bad.append(f"origin/{ref}:{tp} sha256 {got[:12]} != stopgap {str(want)[:12]}")
    led = show(rev, f"{SK}/LEDGER.md") or ""
    row = [r for r in ledger_rows(led) if "**LW3.DIG-CUST-1 " in r]
    if len(row) != 1:
        bad.append(f"LW3.DIG-CUST-1 ledger row: {len(row)} (want 1)")
    else:
        for ref, (d, _, files) in CUST.items():
            for sg in files:
                h = sums.get(f"{d}/{sg}", "")
                if h[:16] not in row[0]:
                    bad.append(f"ledger row does not cite sha256 of {d}/{sg}")
    return bad


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 2
    unit, rev = sys.argv[1], sys.argv[2]
    fn = {"rulings": check_rulings, "fold": check_fold, "status": check_status,
          "tree": check_tree, "cust": check_cust}.get(unit)
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
