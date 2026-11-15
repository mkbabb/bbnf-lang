#!/usr/bin/env python3
"""PERF-1d: the `bbnf` metadata dependency rows of w3_replay_public_contract
(SK_V27_BBNF_DEPENDENCY_ROWS_SHA256), rebuilt from `cargo metadata` JSON on stdin in
the test's exact row form. `rows` prints them; `sha` prints their sha256."""
import hashlib, json, sys
meta = json.load(sys.stdin)
pkg = next(p for p in meta["packages"] if p["name"] == "bbnf" and (p.get("source") is None))
rows = set()
for d in pkg["dependencies"]:
    rows.add("\t".join([d["name"], d["req"], d.get("kind") or "normal",
        str(d["uses_default_features"]).lower(), ",".join(sorted(d["features"])),
        str(d["optional"]).lower(), d.get("target") or "", d.get("source") or "path"]))
text = "".join(r + "\n" for r in sorted(rows))
print(text, end="") if sys.argv[1] == "rows" else print(hashlib.sha256(text.encode()).hexdigest())
