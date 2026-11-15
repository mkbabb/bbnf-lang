#!/usr/bin/env python3
"""LW3.HM-PREWORK INVENTORY re-seal proof (RULINGS 2026-11-14j INVENTORY, as at GEN-RECURSE).

Reads `cargo metadata --locked --offline --format-version 1` (stdin), rebuilds
the sealed metadata identity of w3_replay_public_contract's
cargo_graph_build_feature_surfaces_and_generated_products_are_closed exactly as
the law does (the resolve-node metadata identity rows), and prints the digest
with and without `bbnf-oracle`'s new test target `mapping_census`. The re-seal
is the sole-difference one when the "without" digest equals the value sealed
before this unit (329e1ea6..., GEN-RECURSE).
"""
import hashlib, json, sys
m = json.load(sys.stdin)
by_id = {p["id"]: p for p in m["packages"]}
def sha(rows): return hashlib.sha256("".join(r + "\n" for r in sorted(set(rows))).encode()).hexdigest()
def drop(p, t): return p["name"] == "bbnf-oracle" and p["source"] is None and t["name"] == "mapping_census"
for without in (False, True):
    ident = []
    for node in m["resolve"]["nodes"]:
        p = by_id[node["id"]]
        feats = ",".join(sorted(node["features"]))
        ts = sorted("{}:{}:{}:{}:{}:{}:{}".format(t["name"], ",".join(t["kind"]), ",".join(t["crate_types"]), t["edition"],
                    str(t["doc"]).lower(), str(t["doctest"]).lower(), str(t["test"]).lower())
                    for t in p["targets"] if not (without and drop(p, t)))
        ident.append("{}\t{}\t{}\t{}\t{}".format(p["name"], p["version"], p["source"] or "", feats, "|".join(ts)))
    print(("without" if without else "with   "), "mapping_census: identity", sha(ident))
