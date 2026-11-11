#!/usr/bin/env python3
"""OR-7 G-R2 pre-measurement read (S11): would the registered pairs survive PREREG §11's
discard rule? Reads the spike's own fidelity records (route-{a,b}-r{1,2}.json), restricted to
the DFA tier (compiled on both routes), and compares every match end between the routes."""
import hashlib, json, sys
rec = sys.argv[1]
def load(n): return {l["id"]: l for l in json.load(open(f"{rec}/{n}.json"))["leaves"]}
A1, A2, B1, B2 = (load(n) for n in ("route-a-r1", "route-a-r2", "route-b-r1", "route-b-r2"))
dfa = sorted(k for k in A1 if "results" in A1[k] and "results" in B1.get(k, {}))
ck = lambda R, ks: hashlib.sha256(json.dumps([R[k]["results"] for k in ks], sort_keys=True).encode()).hexdigest()
dis = [k for k in dfa if A1[k]["results"] != B1[k]["results"]]
agree = [k for k in dfa if k not in dis]
print(json.dumps({
  "dfa_leaves": len(dfa), "cases": sum(len(A1[k]["results"]) for k in dfa),
  "route_reps_stable": ck(A1, dfa) == ck(A2, dfa) and ck(B1, dfa) == ck(B2, dfa),
  "checksum_A_all": ck(A1, dfa)[:16], "checksum_B_all": ck(B1, dfa)[:16],
  "pairs_survive_discard_rule": ck(A1, dfa) == ck(B1, dfa),
  "leaves_disagreeing": dis,
  "cases_disagreeing": sum(sum(A1[k]["results"][c] != B1[k]["results"][c] for c in A1[k]["results"]) for k in dis),
  "agreeing_leaves": len(agree), "checksum_agreeing_A": ck(A1, agree)[:16], "checksum_agreeing_B": ck(B1, agree)[:16],
  "find_unanchored_implemented": {"route_a": False, "route_b": False},
}, indent=1))
