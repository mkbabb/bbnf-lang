#!/usr/bin/env python3
"""PERF-1d: w3_replay_public_contract's complete metadata identity rows
(SK_V27_METADATA_IDENTITY_SHA256) from `cargo metadata` JSON on stdin, in the
test's exact row form. `rows` prints them; `sha` prints their sha256."""
import hashlib, json, sys
meta = json.load(sys.stdin)
by_id = {p["id"]: p for p in meta["packages"]}
rows = set()
for node in meta["resolve"]["nodes"]:
    p = by_id[node["id"]]
    feats = sorted(node["features"])
    targets = sorted("%s:%s:%s:%s:%s:%s:%s" % (t["name"], ",".join(t["kind"]), ",".join(t["crate_types"]),
        t["edition"], str(t["doc"]).lower(), str(t["doctest"]).lower(), str(t["test"]).lower()) for t in p["targets"])
    rows.add("\t".join([p["name"], p["version"], p.get("source") or "", ",".join(feats), "|".join(targets)]))
text = "".join(r + "\n" for r in sorted(rows))
print(text, end="") if sys.argv[1] == "rows" else print(hashlib.sha256(text.encode()).hexdigest())
