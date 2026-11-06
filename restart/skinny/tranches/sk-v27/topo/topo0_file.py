#!/usr/bin/env python3
"""LW1.TOPO-0: file the seven adjudicated design texts from the LW1 formation
workflow journal (wf_950f38d8, the `adjudicate:*` results), byte for byte."""
import hashlib, json, pathlib, sys

journal = pathlib.Path(sys.argv[1])
out = pathlib.Path(__file__).resolve().parent.parent / "designs"
out.mkdir(exist_ok=True)
lines = [json.loads(l) for l in journal.open()]
label = {o["key"]: (o["label"], o["agentId"]) for o in lines if o.get("type") == "started"}
rows = ["name\tjournal_line\tagent\tbytes\tsha256"]
for i, o in enumerate(lines):
    lab, agent = label.get(o.get("key"), ("", ""))
    if o.get("type") == "result" and lab.startswith("adjudicate:"):
        name = lab.split(":", 1)[1]
        b = o["result"].encode()
        (out / f"{name}.md").write_bytes(b)
        rows.append(f"{name}\t{i + 1}\t{agent}\t{len(b)}\t{hashlib.sha256(b).hexdigest()}")
(out / "SOURCE.tsv").write_text("\n".join(sorted(rows, key=lambda r: (r[:4] != "name", r))) + "\n")
print(len(rows) - 1, "texts filed")
