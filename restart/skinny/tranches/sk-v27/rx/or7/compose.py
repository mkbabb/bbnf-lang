#!/usr/bin/env python3
"""sk-v27 LW2.RX-1: compose rx/OR7-SPIKE.md from the spike's records.

Inputs (all written by the spike, see rx/or7/README in OR7-SPIKE.md):
  --census  rx/LOOKAROUND-CENSUS.tsv   leaf ids, patterns, lookaround tier
  --answers <oracle answers json>      V8+JSC answers per case id
  --route   <route results json> (x2)  per leaf: compile_us, states, results | error
  --meta    <json>                     host facts (load, engines, pins, shas)
  --out     rx/OR7-SPIKE.md

Per leaf per route, every case with an oracle answer is compared:
  - oracle MATCH with a UTF-8 end: the route must return that end;
  - oracle NO_MATCH: the route must return no match;
  - oracle SPEC-AMBIGUITY (V8 and JSC differ): counted, never compared;
  - oracle MATCH ending inside a surrogate pair (non-u, astral input): no
    UTF-8 image, counted as ENCODING-EXCLUDED, never compared.
A leaf the route rejects is ENGINE when the oracle compiles the pattern.
Verdict per row: ENGINE if any compared case differs (or the route rejects a
pattern the oracle accepts); else SPEC-AMBIGUITY if any case is ambiguous;
else AGREE. Lookaround leaves are their own tier and are never pooled.
"""
import argparse
import json
import statistics


def load(path):
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--census", required=True)
    ap.add_argument("--answers", required=True)
    ap.add_argument("--route", action="append", required=True)
    ap.add_argument("--meta", required=True)
    ap.add_argument("--out", required=True)
    a = ap.parse_args()

    leaves = []
    with open(a.census, encoding="utf-8") as f:
        head = f.readline().rstrip("\n").split("\t")
        for line in f:
            row = dict(zip(head, line.rstrip("\n").split("\t")))
            leaves.append(row)
    answers = load(a.answers)
    routes = [load(p) for p in a.route]
    meta = load(a.meta)

    def tier(leaf):
        return "LOOKAROUND" if leaf["lookaround"] == "1" else "DFA"

    rows = []  # (tier, leaf, route name, cells...)
    examples = {}
    for route in routes:
        by_id = {r["id"]: r for r in route["leaves"]}
        for leaf in leaves:
            lid = f'{leaf["corpus"]}#{leaf["n"]}'
            rec = by_id[lid]
            prefix = lid + "|"
            case_ids = [k for k in answers if k.startswith(prefix)]
            oracle_compiles = any(answers[k].get("status") != "SYNTAX_ERROR" for k in case_ids)
            agree = engine = amb = enc = 0
            first_diff = None
            if "error" in rec:
                verdict = "ENGINE" if oracle_compiles else "AGREE"
                note = "rejects: " + rec["error"]
            else:
                for k in case_ids:
                    ans = answers[k]
                    st = ans.get("status")
                    if st == "SPEC-AMBIGUITY":
                        amb += 1
                        continue
                    if st == "MATCH" and ans.get("end8") is None:
                        enc += 1
                        continue
                    want = ans["end8"] if st == "MATCH" else None
                    got = rec["results"].get(k)
                    if got == want:
                        agree += 1
                    else:
                        engine += 1
                        if first_diff is None:
                            first_diff = (k, want, got)
                verdict = "ENGINE" if engine else ("SPEC-AMBIGUITY" if amb else "AGREE")
                note = ""
                if first_diff:
                    k, want, got = first_diff
                    examples[(lid, route["route"])] = (k, want, got)
                    note = f"first: case {k.split('|')[1]} oracle {want} route {got}"
            rows.append({
                "tier": tier(leaf), "leaf": lid, "pattern": leaf["pattern"], "flags": leaf["flags"],
                "route": route["route"][0], "compiled": "error" not in rec,
                "states": rec.get("states", ""), "compile_us": rec["compile_us"],
                "cases": len(case_ids), "agree": agree, "engine": engine, "amb": amb, "enc": enc,
                "verdict": verdict, "note": note,
            })

    def esc(s):
        return str(s).replace("|", "\\|").replace("\n", "\\n")

    out = []
    out.append("# OR-7 spike: Route A (regex-automata) against Route B (in-house)\n")
    out.append("sk-v27 LW2.RX-1 (seat S7). One row per leaf per route, each with its oracle verdict. "
               "The leaves are the 115 distinct value.js leaves plus the sk-v24 56 "
               "(`rx/LOOKAROUND-CENSUS.tsv`). The lookaround leaves are a tier of their own and are "
               "never pooled with the DFA tier (C1-9). Emitted-code speed is not measured here; it is "
               "queued as Phase 3 cells (PREREG F-REGEX). S14 turns this record into the OR-7 ruling ask.\n")
    out.append("## Method\n")
    for line in meta["method"]:
        out.append(f"- {line}")
    out.append("")
    out.append("## Host and pins\n")
    for k, v in meta["host"].items():
        out.append(f"- **{k}:** {v}")
    out.append("")

    out.append("## Summary by route and tier\n")
    out.append("| Route | Tier | Leaves | Compiled | AGREE | ENGINE | SPEC-AMBIGUITY | Cases compared | Case disagreements | Median compile µs (compiled) | Median states (compiled) |")
    out.append("|---|---|---|---|---|---|---|---|---|---|---|")
    for route in routes:
        rn = route["route"][0]
        for t in ("DFA", "LOOKAROUND"):
            rs = [r for r in rows if r["route"] == rn and r["tier"] == t]
            comp = [r for r in rs if r["compiled"]]
            med_us = f'{statistics.median([r["compile_us"] for r in comp]):.1f}' if comp else "—"
            med_st = f'{statistics.median([r["states"] for r in comp]):.0f}' if comp else "—"
            out.append(
                f"| {rn} | {t} | {len(rs)} | {len(comp)} | {sum(r['verdict']=='AGREE' for r in rs)} | "
                f"{sum(r['verdict']=='ENGINE' for r in rs)} | {sum(r['verdict']=='SPEC-AMBIGUITY' for r in rs)} | "
                f"{sum(r['agree']+r['engine'] for r in rs)} | {sum(r['engine'] for r in rs)} | {med_us} | {med_st} |")
    out.append("")
    out.append("Route names: " + "; ".join(f'**{r["route"][0]}** = {r["route"]}' for r in routes) + ".\n")

    # Where the routes part.
    by = {(r["leaf"], r["route"]): r for r in rows}
    names = [r["route"][0] for r in routes]
    parted = [l for l in dict.fromkeys(r["leaf"] for r in rows)
              if len({by[(l, n)]["verdict"] for n in names}) > 1]
    out.append(f"## Leaves where the routes' verdicts differ ({len(parted)})\n")
    out.append("| Leaf | Tier | Pattern | " + " | ".join(f"Route {n}" for n in names) + " |")
    out.append("|---|---|---|" + "---|" * len(names))
    for l in parted:
        r0 = by[(l, names[0])]
        cells = [f'{by[(l, n)]["verdict"]} ({by[(l, n)]["engine"]}/{by[(l, n)]["agree"]+by[(l, n)]["engine"]}) {esc(by[(l, n)]["note"])}' for n in names]
        out.append(f"| {l} | {r0['tier']} | `{esc(r0['pattern'])}` | " + " | ".join(cells) + " |")
    out.append("")

    for t in ("DFA", "LOOKAROUND"):
        out.append(f"## Rows: {t} tier\n")
        out.append("| Leaf | Route | Flags | Compiled | States | Compile µs | Cases | Agree | Engine | Spec-amb. | Enc.-excl. | Verdict | Note |")
        out.append("|---|---|---|---|---|---|---|---|---|---|---|---|---|")
        for r in rows:
            if r["tier"] != t:
                continue
            out.append(
                f"| {r['leaf']} | {r['route']} | {r['flags'] or '—'} | {'yes' if r['compiled'] else 'no'} | "
                f"{r['states']} | {r['compile_us']:.1f} | {r['cases']} | {r['agree']} | {r['engine']} | "
                f"{r['amb']} | {r['enc']} | {r['verdict']} | {esc(r['note'])} |")
        out.append("")

    out.append("## Patterns\n")
    out.append("| Leaf | Tier | Flags | Pattern |")
    out.append("|---|---|---|---|")
    for leaf in leaves:
        out.append(f'| {leaf["corpus"]}#{leaf["n"]} | {tier(leaf)} | {leaf["flags"] or "—"} | `{esc(leaf["pattern"])}` |')
    out.append("")
    with open(a.out, "w", encoding="utf-8") as f:
        f.write("\n".join(out))


if __name__ == "__main__":
    main()
