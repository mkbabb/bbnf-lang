import json, statistics, sys
p = "/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/scratchpad/PROVE-11-work/paired-samples.jsonl"
rows = [json.loads(l) for l in open(p) if l.strip()]
lanes = ["fusedNs","unfusedNs","jsonNs","dispatchNs","seqAnyNs","fusedAllocBpo","jsonAllocBpo"]
label = {"fusedNs":"fused all(a,b,c)","unfusedNs":"unfused then-chain","jsonNs":"json-comprehensive",
         "dispatchNs":"first-char dispatch","seqAnyNs":"sequential any","fusedAllocBpo":"fused alloc B/op","jsonAllocBpo":"json alloc B/op"}
print(f"N rounds = {len(rows)}   loadavg samples: {[r['loadavg'] for r in rows]}")
print()
print(f"{'lane':<22}{'pristine(med)':>16}{'modified(med)':>16}{'ratio pri/mod':>16}{'Δ% (mod win)':>14}")
for k in lanes:
    pv = sorted(r["pristine"][k] for r in rows)
    mv = sorted(r["modified"][k] for r in rows)
    pmed = statistics.median(pv); mmed = statistics.median(mv)
    ratio = pmed/mmed
    win = (1 - mmed/pmed)*100  # positive = modified faster/lighter
    unit = "B" if "Alloc" in k else "ns"
    print(f"{label[k]:<22}{pmed:>14.2f}{unit:>2}{mmed:>14.2f}{unit:>2}{ratio:>15.3f}x{win:>13.1f}%")
print()
# per-round ratio distribution for the headline lanes (paired-ish: same round = same load window)
for k in ["fusedNs","jsonNs"]:
    rr = sorted(r["pristine"][k]/r["modified"][k] for r in rows)
    print(f"{label[k]} per-round pri/mod ratios: min {rr[0]:.3f} med {statistics.median(rr):.3f} max {rr[-1]:.3f}")
