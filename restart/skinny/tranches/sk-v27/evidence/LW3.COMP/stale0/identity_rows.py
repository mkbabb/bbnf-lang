#!/usr/bin/env python3
"""sk-v27 LW3.STALE-0 (i): the metadata-identity rows of
`w3_replay_public_contract::cargo_graph_build_feature_surfaces_and_generated_products_are_closed`,
rebuilt outside the test from `cargo metadata --locked --offline --format-version 1` JSON on
stdin, in the test's exact row form. Prints the rows (`rows`) or their sha256 (`sha`).
With `--add PKG TARGETROW` the named target row is added to PKG's target list first (to
re-derive the pinned hash from the present graph)."""
import hashlib, json, sys

def rows(meta, add=None):
    by_id = {p["id"]: p for p in meta["packages"]}
    out = set()
    for node in meta["resolve"]["nodes"]:
        p = by_id[node["id"]]
        feats = sorted(node["features"])
        targets = [
            "%s:%s:%s:%s:%s:%s:%s" % (
                t["name"], ",".join(t["kind"]), ",".join(t["crate_types"]), t["edition"],
                str(t["doc"]).lower(), str(t["doctest"]).lower(), str(t["test"]).lower())
            for t in p["targets"]]
        if add and p["name"] == add[0]:
            targets.append(add[1])
        targets.sort()
        out.add("%s\t%s\t%s\t%s\t%s" % (p["name"], p["version"], p.get("source") or "",
                                        ",".join(feats), "|".join(targets)))
    return "".join(r + "\n" for r in sorted(out))

args = sys.argv[1:]
mode = args.pop(0)
add = (args[1], args[2]) if args[:1] == ["--add"] else None
text = rows(json.load(sys.stdin), add)
print(hashlib.sha256(text.encode()).hexdigest() if mode == "sha" else text, end="" if mode == "rows" else "\n")
