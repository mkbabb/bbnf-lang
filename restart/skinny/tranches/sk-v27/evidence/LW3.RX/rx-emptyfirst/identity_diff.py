#!/usr/bin/env python3
"""LW3.RX-EMPTYFIRST INVENTORY re-seal proof (RULINGS 2026-11-14j INVENTORY).

Reads `cargo metadata --locked --offline --format-version 1` (stdin), rebuilds
the two sealed identities of w3_replay_public_contract's
cargo_graph_build_feature_surfaces_and_generated_products_are_closed exactly as
the law does (the bbnf target rows; the resolve-node metadata identity rows),
and prints each digest with and without the `rx_emptyfirst` test target. The
re-seal is the sole-difference one when the "without" digests equal the values
sealed before this unit.
"""
import hashlib, json, sys
m = json.load(sys.stdin)
by_id = {p["id"]: p for p in m["packages"]}
def sha(rows): return hashlib.sha256("".join(r + "\n" for r in sorted(set(rows))).encode()).hexdigest()
def drop(t): return t["name"] == "rx_emptyfirst"
for without in (False, True):
    bbnf = [p for p in m["packages"] if p["name"] == "bbnf" and p["source"] is None][0]
    trows = [t["name"] + ":" + ",".join(t["kind"]) for t in bbnf["targets"] if not (without and drop(t))]
    ident = []
    for node in m["resolve"]["nodes"]:
        p = by_id[node["id"]]
        feats = ",".join(sorted(node["features"]))
        ts = sorted("{}:{}:{}:{}:{}:{}:{}".format(t["name"], ",".join(t["kind"]), ",".join(t["crate_types"]), t["edition"],
                    str(t["doc"]).lower(), str(t["doctest"]).lower(), str(t["test"]).lower())
                    for t in p["targets"] if not (without and p["name"] == "bbnf" and p["source"] is None and drop(t)))
        ident.append("{}\t{}\t{}\t{}\t{}".format(p["name"], p["version"], p["source"] or "", feats, "|".join(ts)))
    print(("without" if without else "with   "), "rx_emptyfirst: targets", len(set(trows)), sha(trows), "identity", sha(ident))
