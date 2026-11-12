#!/usr/bin/env python3
"""LW3 Phase 3a record units of seat S7 SPINE (after RULINGS 2026-11-12d).

Usage: p3a_check.py <unit> <rev>
  unit: rulings | ledger
  rev:  the bbnf-lang commit read (files are read with `git show <rev>:<path>`).
Exit 0 = GREEN, 1 = RED (each reason printed), 2 = usage.

rulings  RULINGS.md carries `## Addendum, 2026-11-12d`, one row per ruling, each
         ruled row marked "delegate, owner may overturn" and holding its named
         facts; DP-3 is recorded as unanswered; the text before the addendum is
         unchanged (append-only against BASE).
ledger   LEDGER.md carries, once each and with a status cell, the rows the
         2026-11-12d rulings order: the two born-RED gate rows (COMP1B-W3 (b)
         and (c)), the COMP-1d item for (b), the COMP-1b-W reviewer row, the
         `[[test]]` drift row, the semver row and the TREE-WRITE census row;
         the sealed text before them is unchanged.
"""
import re, subprocess, sys

SK = "restart/skinny/tranches/sk-v27"
BASE = "7eba692ee26bcd4b9b853db38ff87130b683a9f3"  # sk-v27-lw3 when this seat opened
MARK = "delegate, owner may overturn"


def git(*a, check=True):
    p = subprocess.run(["git", *a], capture_output=True, text=True)
    if check and p.returncode != 0:
        raise RuntimeError(f"git {' '.join(a)}: {p.stderr.strip()}")
    return p


def show(rev, path):
    p = git("show", f"{rev}:{path}", check=False)
    return p.stdout if p.returncode == 0 else ""


def check_rulings(rev):
    bad = []
    t = show(rev, f"{SK}/RULINGS.md")
    old = show(BASE, f"{SK}/RULINGS.md")
    if not t.startswith(old.rstrip("\n")):
        bad.append("RULINGS.md: text before the addendum changed (not append-only)")
    m = re.search(r"^## Addendum, 2026-11-12d\b.*?(?=^## |\Z)", t, re.M | re.S)
    if not m:
        return bad + ["RULINGS.md has no `## Addendum, 2026-11-12d` section"]
    sec = m.group(0)
    if MARK not in sec.split("\n| # |")[0]:
        bad.append("2026-11-12d: the preamble does not say every ruled row is a delegate ruling")
    if "wave 3 of 7" not in sec:
        bad.append("2026-11-12d: the preamble does not say LW3 is wave 3 of 7")
    want = {
        "COMP1B-W3": [
            "cargo_graph_build_feature_surfaces_and_generated_products_are_closed",
            "exact_packages_cross_a_fresh_local_registry_and_public_runtime",
            "W3-PARSE-THAT-ABI3.json", "ABI-4 receipt", "LW3.COMP-1b-T",
            "born RED", "×2", "at least as much", "assertion by assertion",
            "active_roots_remain_preselection_only_and_the_guard_self_falsifies",
            "cured in LW3 by COMP-1d", '#[ignore = "LW3.COMP-1d"]', "census",
            "LG-3 cannot close while it is ignored",
            "every_core_and_external_control_caller_uses_only_the_clean_break_instance_api",
            "LW7.COMP-1", "re-trigger row", "OPEN carry the owner must see"],
        "LG-3.1": ["not relaxed", "unruled=0", "per grammar per arm", "LW3-ORDER.md:224",
                   "root arm", "ORC2-WS-C", "CSS root rows", "read fresh", "TS-package arm",
                   "J-12, J-13, J-17, J-18", "exception table", "born-RED test",
                   "divergences minus the listed ids = 0", "prints the exception list",
                   "owner-visible carry"],
        "COMP-1b-W": ["provisionally accepted", "four components deep", "reviewer's read",
                      "before LW5 relies on it"],
        "TC-COUNT": ["order drift", "13", "11", "b5fd0ca98", "instance_support",
                     "crates/core/tests/instance_support.rs", "shape_dispatch_fixtures",
                     "crates/core/tests/shape_dispatch_emission/fixtures.rs", "Nothing was dropped"],
        "TREE-WRITE": ["must not dirty the tree", "crates/core/benches/ts/generated_json.mjs",
                       "gen_ts_parser.rs", "docs/tranches/AZ-IV/audit/W5-node-execute.txt",
                       "ts_node_execute.rs", "census row", "temporary directory", "Phase 3b1"],
        "SEMVER-COPY": ["no removal", "copy_impl_added", "five", "Copy", "master's code",
                        "semver-probe.sh", "nothing is decided here"],
    }
    for rid, needles in want.items():
        rows = [l for l in sec.split("\n") if l.startswith(f"| {rid} |")]
        if len(rows) != 1:
            bad.append(f"{rid}: {len(rows)} rows in 2026-11-12d (want 1)")
            continue
        if MARK not in rows[0]:
            bad.append(f"{rid}: row is not marked \"{MARK}\"")
        for n in needles:
            if n not in rows[0]:
                bad.append(f"{rid}: row lacks {n!r}")
    dp3 = [l for l in sec.split("\n") if l.startswith("| DP-3 |")]
    if len(dp3) != 1:
        bad.append(f"DP-3: {len(dp3)} rows (want 1)")
    elif not all(n in dp3[0] for n in ("UNANSWERED", "owner", "R1")):
        bad.append("DP-3: row does not record the ask as unanswered and the owner's")
    return bad


STATUS = re.compile(r"\| (RED|GREEN|PLANNED|RULING|OPEN|CARRIED|RECORDED|CLOSED)[^|]*\|\s*$")


def one_row(led, title, status, needles):
    hit = [l for l in led.split("\n") if l.startswith("| 2026-") and f"**{title}" in l]
    if len(hit) != 1:
        return [f"{title}: {len(hit)} LEDGER rows (want 1)"]
    bad = []
    m = STATUS.search(hit[0])
    if not m:
        bad.append(f"{title}: row has no status cell")
    elif m.group(1) != status:
        bad.append(f"{title}: status {m.group(1)} (want {status})")
    for n in needles:
        if n not in hit[0]:
            bad.append(f"{title}: row lacks {n!r}")
    return bad


def check_ledger(rev):
    led = show(rev, f"{SK}/LEDGER.md")
    old = show(BASE, f"{SK}/LEDGER.md")
    bad = []
    if not led.startswith(old.rstrip("\n")):
        bad.append("LEDGER.md: sealed text changed (not append-only)")
    rows = [
        ("LW3.COMP-1b-BR1", "OPEN", [
            "active_roots_remain_preselection_only_and_the_guard_self_falsifies",
            '#[ignore = "LW3.COMP-1d"]', "COMP-1d (vi)", "crates/core/grammar/",
            "symlink", "census", "LG-3 cannot close", "2026-11-12d"]),
        ("LW3.COMP-1b-BR2", "OPEN", [
            "every_core_and_external_control_caller_uses_only_the_clean_break_instance_api",
            "LW7.COMP-1", "re-trigger", "OPEN carry", "owner", "census", "2026-11-12d"]),
        ("LW3.COMP-1d amended", "PLANNED", [
            "(vi)", "active_roots_remain_preselection_only_and_the_guard_self_falsifies",
            "crates/core/grammar/", "reverse-dependency", "symlink", "loses `#[ignore]`",
            "2026-11-12d"]),
        ("LW3.COMP-1b-W review owed", "OPEN", [
            "four components deep", "reviewer", "LW5", "2026-11-12d"]),
        ("LW3.COMP-1b test-target drift", "RECORDED", [
            "13", "11", "instance_support", "shape_dispatch_fixtures",
            "shape_dispatch_emission/fixtures.rs", "2026-11-12d"]),
        ("LW3.COMP-1b semver finding", "RECORDED", [
            "copy_impl_added", "BbnfStructCheckpoint", "SimpleCheckpoint", "517a587e9",
            "semver-probe.sh", "2026-11-12d"]),
        ("LW3.TREE-1 census", "PLANNED", [
            "gen_ts_parser.rs", "generated_json.mjs", "ts_node_execute.rs",
            "W5-node-execute.txt", "git status --porcelain", "2026-11-12d"]),
    ]
    for title, status, needles in rows:
        bad += one_row(led, title, status, needles)
    return bad


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 2
    unit, rev = sys.argv[1], sys.argv[2]
    fn = {"rulings": check_rulings, "ledger": check_ledger}.get(unit)
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
