#!/usr/bin/env python3
"""Cell admission for the contest (sk-v27 LW2.CON-1; competition U4 falsifiers (c) and (d)).

A cell is measured only after it is admitted. Admission refuses, and names each reason:

1. a cell that `bench/PREREG.md` does not register, or a PREREG.md whose digest no longer
   matches `PREREG.md.sha256`;
2. an arm with no row in `bench/contest/ROWS.tsv`, a row marked "no comparator", or two
   rows of different planes or output-parity classes;
3. **plane inequivalence**: the two arms disagree on any witness input of the plane
   (`bench/planes/witness/<corpus>/`), in the fields the class compares;
4. **build-plane mismatch**: a build-plane record is missing, lacks a field the I-8 manifest
   requires, or differs from the other arm's in a field the pair must share.

Usage:
  admit.py cell CELL_ID [--build-a FILE --build-b FILE]
  admit.py pair --plane P --class K --a CMD --b CMD [--build-a FILE --build-b FILE]

Exit status 0 means ADMITTED; 1 means REFUSED (the reasons are printed).
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import re
import shlex
import subprocess
import sys
from dataclasses import dataclass, field

HERE = os.path.dirname(os.path.abspath(__file__))
BENCH = os.path.dirname(HERE)
REPO = os.path.dirname(BENCH)
ROWS = os.path.join(HERE, "ROWS.tsv")
PREREG = os.path.join(BENCH, "PREREG.md")
PREREG_DIGEST = os.path.join(BENCH, "PREREG.md.sha256")
WITNESS = os.path.join(BENCH, "planes", "witness")

# Plane -> the witness corpus it is checked on (bench/planes/P*.md, "Witness inputs").
PLANE_WITNESS = {"P1": "json", "P3": "json", "P4": "json", "P5": "json", "P6": "json",
                 "P2": "css", "P7": "css"}
# Output-parity class -> what two arms must agree on, per witness input.
#   verdict: ACCEPT or REJECT;  digest: the canonical-form digest after ACCEPT;
#   offset:  the error offset after REJECT.
CLASS_FIELDS = {"CHK": ("verdict", "offset"), "TOK": ("verdict", "digest"), "TAPE": ("verdict", "digest"),
                "VAL": ("verdict", "digest"), "CST": ("verdict", "digest"), "GET": ("verdict", "digest"),
                "TYPED": ("verdict", "digest")}
NO_COMPARATOR = "NONE"

# The I-8 build plane, per language; `matrix` is PORTABLE or NATIVE.
BUILD_FIELDS = {
    "rust": ("toolchain", "llvm", "opt_level", "lto", "codegen_units", "panic", "target_cpu", "allocator", "matrix"),
    "c": ("toolchain", "llvm", "opt_level", "lto", "target_cpu", "allocator", "matrix"),
    "js": ("toolchain", "runtime_flags", "matrix"),
}
# Fields an arm pair in two languages must still share (the native pair's codegen plane).
CROSS_LANGUAGE_FIELDS = ("llvm", "opt_level", "lto", "target_cpu", "allocator", "matrix")


@dataclass
class Verdict:
    admitted: bool = True
    reasons: list[str] = field(default_factory=list)

    def refuse(self, reason: str) -> None:
        self.admitted = False
        self.reasons.append(reason)

    def extend(self, other: "Verdict") -> None:
        for r in other.reasons:
            self.refuse(r)

    def text(self) -> str:
        if self.admitted:
            return "ADMITTED"
        return "REFUSED\n" + "\n".join("  - " + r for r in self.reasons)


# ── registry ──────────────────────────────────────────────────────────────────

def rows() -> dict[str, dict]:
    with open(ROWS, encoding="utf-8", newline="") as f:
        lines = [ln for ln in f if not ln.startswith("#")]
    return {r["row"]: r for r in csv.DictReader(lines, delimiter="\t")}


def prereg_cells() -> dict[str, dict]:
    """Registered contest cells: the `| C-… |` rows of PREREG.md's arm-pair tables (§7), keyed by id.

    Only a table with `Arm (bbnf)` and `Arm (opp)` columns registers a pair this gate admits; the
    §6 root-trial cells name no arm pair and are run and refused by `bench/cb-b0/cbb0.py --cell`."""
    with open(PREREG, encoding="utf-8") as f:
        text = f.read()
    cells: dict[str, dict] = {}
    header: list[str] | None = None
    for line in text.splitlines():
        if not line.startswith("|"):
            header = None
            continue
        cols = [c.strip() for c in line.strip().strip("|").split("|")]
        if cols and cols[0] == "Cell":
            header = [c.lower() for c in cols]
            if not {"arm (bbnf)", "arm (opp)"} <= set(header):
                header = None
        elif header and re.fullmatch(r"C-[A-Z0-9-]+", cols[0]):
            cells[cols[0]] = dict(zip(header, cols))
    return cells


def prereg_digest_ok() -> bool:
    with open(PREREG, "rb") as f:
        got = hashlib.sha256(f.read()).hexdigest()
    with open(PREREG_DIGEST) as f:
        return got == f.read().split()[0]


# ── (c) plane equivalence ─────────────────────────────────────────────────────

def witness_files(plane: str) -> list[str]:
    if plane not in PLANE_WITNESS:
        raise KeyError(plane)
    d = os.path.join(WITNESS, PLANE_WITNESS[plane])
    return sorted(os.path.join(d, n) for n in os.listdir(d) if not n.startswith("."))


def read_arm(cmd: str, files: list[str]) -> list[tuple[str, str]]:
    """Run an arm's witness mode once over every file; one (verdict, detail) per file."""
    out = subprocess.run(shlex.split(cmd) + files, cwd=REPO, capture_output=True, text=True, timeout=600)
    lines = out.stdout.splitlines()
    if out.returncode != 0 or len(lines) != len(files):
        raise RuntimeError(f"arm `{cmd}` exited {out.returncode} with {len(lines)} lines for "
                           f"{len(files)} inputs: {out.stderr.strip()[:300]}")
    res = []
    for ln in lines:
        verdict, _, detail = ln.partition(" ")
        if verdict not in ("ACCEPT", "REJECT"):
            raise RuntimeError(f"arm `{cmd}` printed {ln!r}")
        res.append((verdict, detail))
    return res


def plane_parity(plane: str, klass: str, cmd_a: str, cmd_b: str) -> Verdict:
    v = Verdict()
    if klass not in CLASS_FIELDS:
        v.refuse(f"class {klass} has no comparator on plane {plane}")
        return v
    if plane not in PLANE_WITNESS:
        v.refuse(f"plane {plane} has no spec and no witness inputs")
        return v
    files = witness_files(plane)
    try:
        a, b = read_arm(cmd_a, files), read_arm(cmd_b, files)
    except RuntimeError as e:
        v.refuse(f"plane-inequivalent: {e}")
        return v
    fields = CLASS_FIELDS[klass]
    for path, (va, da), (vb, db) in zip(files, a, b):
        name = os.path.basename(path)
        if va != vb:
            v.refuse(f"plane-inequivalent on {plane}/{name}: {va} vs {vb}")
        elif va == "ACCEPT" and "digest" in fields and da != db:
            v.refuse(f"plane-inequivalent on {plane}/{name}: output digest {da[:12]} vs {db[:12]}")
        elif va == "REJECT" and "offset" in fields:
            if "-" in (da, db):
                v.refuse(f"plane-inequivalent on {plane}/{name}: an arm reports no error offset ({klass} compares it)")
            elif da != db:
                v.refuse(f"plane-inequivalent on {plane}/{name}: error offset {da} vs {db}")
    return v


# ── (d) build-plane equality ──────────────────────────────────────────────────

def build_parity(bp_a: dict | None, bp_b: dict | None) -> Verdict:
    v = Verdict()
    for side, bp in (("a", bp_a), ("b", bp_b)):
        if bp is None:
            v.refuse(f"build plane of arm {side} missing")
            continue
        lang = bp.get("lang")
        if lang not in BUILD_FIELDS:
            v.refuse(f"build plane of arm {side}: lang {lang!r} not in {sorted(BUILD_FIELDS)}")
            continue
        for k in BUILD_FIELDS[lang]:
            if k not in bp or bp[k] in ("", None):
                v.refuse(f"build plane of arm {side} lacks {k}")
    if not v.admitted:
        return v
    shared = BUILD_FIELDS[bp_a["lang"]] if bp_a["lang"] == bp_b["lang"] else tuple(
        k for k in CROSS_LANGUAGE_FIELDS if k in BUILD_FIELDS[bp_a["lang"]] and k in BUILD_FIELDS[bp_b["lang"]])
    for k in shared:
        if bp_a[k] != bp_b[k]:
            v.refuse(f"build-plane mismatch: {k} {bp_a[k]!r} vs {bp_b[k]!r}")
    return v


def load_build_plane(path: str | None) -> dict | None:
    if path is None:
        return None
    with open(path) as f:
        return json.load(f)


# ── cells ─────────────────────────────────────────────────────────────────────

def admit_cell(cell_id: str, build_a: str | None = None, build_b: str | None = None) -> Verdict:
    """Admit or refuse a registered cell; `build_a`/`build_b` are the two arms' build-plane records."""
    v = Verdict()
    if not prereg_digest_ok():
        v.refuse("PREREG.md does not match PREREG.md.sha256 (an edit after the hash is an addendum)")
    cell = prereg_cells().get(cell_id)
    if cell is None:
        v.refuse(f"cell {cell_id} is not a registered contest cell in PREREG.md §7")
        return v
    table = rows()
    arms = [cell.get("arm (bbnf)"), cell.get("arm (opp)")]
    found = []
    for a in arms:
        row = table.get(a or "")
        if row is None:
            v.refuse(f"arm {a!r} has no row in ROWS.tsv")
        elif row["class"] == NO_COMPARATOR:
            v.refuse(f"row {a} is marked no comparator")
        else:
            found.append(row)
    if len(found) != 2:
        return v
    ra, rb = found
    for key in ("plane", "class"):
        if ra[key] != rb[key] or ra[key] != cell[key]:
            v.refuse(f"{key} differs: cell {cell[key]}, {ra['row']} {ra[key]}, {rb['row']} {rb[key]}")
    if not v.admitted:
        return v
    for r in ({r["row"]: r for r in found}).values():
        if r["witness"] in ("", "-") or r["witness"].startswith("OWED"):
            v.refuse(f"row {r['row']} has no witness mode yet ({r['witness'] or 'empty'})")
    if not v.admitted:
        return v
    v.extend(plane_parity(ra["plane"], ra["class"], ra["witness"], rb["witness"]))
    v.extend(build_parity(load_build_plane(build_a), load_build_plane(build_b)))
    return v


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="verb", required=True)
    c = sub.add_parser("cell")
    c.add_argument("cell")
    c.add_argument("--build-a")
    c.add_argument("--build-b")
    p = sub.add_parser("pair")
    p.add_argument("--plane", required=True, choices=sorted(PLANE_WITNESS))
    p.add_argument("--class", dest="klass", required=True)
    p.add_argument("--a", required=True)
    p.add_argument("--b", required=True)
    p.add_argument("--build-a")
    p.add_argument("--build-b")
    a = ap.parse_args(argv)
    if a.verb == "cell":
        v = admit_cell(a.cell, a.build_a, a.build_b)
    else:
        v = plane_parity(a.plane, a.klass, a.a, a.b)
        if a.build_a or a.build_b:
            v.extend(build_parity(load_build_plane(a.build_a), load_build_plane(a.build_b)))
    print(v.text())
    return 0 if v.admitted else 1


if __name__ == "__main__":
    sys.exit(main())
