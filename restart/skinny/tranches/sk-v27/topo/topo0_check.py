#!/usr/bin/env python3
"""LW1.TOPO-0 falsifier: the seven adjudicated design texts are filed whole.

GREEN iff designs/{7}.md exist, each is byte-identical to the adjudicator's
result recorded in designs/SOURCE.tsv (sha256 + bytes), and each carries the
text past the dispatch cut named in LEDGER §4.
"""
import hashlib, os, pathlib, sys

HERE = pathlib.Path(os.environ.get("TOPO0_DESIGNS") or pathlib.Path(__file__).resolve().parent.parent / "designs")
CUTS = {  # LEDGER §4: where the dispatch copies were cut off
    "compiler": "G1", "rust-perf": "## 1. Target architecture", "regex": "§A",
    "pprint": "**L9, trivia:**", "competition": "L4t TS emitter",
    "consumers": "G-J1", "programme": "**R-8.",
}
bad = []
src = HERE / "SOURCE.tsv"
rows = {}
if src.is_file():
    for line in src.read_text().splitlines()[1:]:
        name, _line, _agent, nbytes, sha = line.split("\t")
        rows[name] = (int(nbytes), sha)
for name, cut in CUTS.items():
    f = HERE / f"{name}.md"
    if not f.is_file():
        bad.append(f"{name}: missing"); continue
    b = f.read_bytes()
    if name not in rows:
        bad.append(f"{name}: no SOURCE.tsv row"); continue
    if (len(b), hashlib.sha256(b).hexdigest()) != rows[name]:
        bad.append(f"{name}: not byte-identical to the adjudicated result"); continue
    if cut not in b.decode():
        bad.append(f"{name}: text past the cut ({cut!r}) absent")
print("RED" if bad else "GREEN", f"{7 - len(bad)}/7", *bad, sep="\n  " if bad else " ")
sys.exit(1 if bad else 0)
