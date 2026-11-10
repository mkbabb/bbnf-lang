"""Writes restart/skinny/tranches/sk-v27/grammar/DIVERGENCE.tsv from the two arms' base readings.

argv: repo-root ts-base.tsv rust-base.tsv out.tsv
Each row is one MANIFEST grammar on which the arms disagree at the base (LW2.PAR-1 entry re-read),
with the constructs that cause it, the spec sentences that rule it, and its fate.
"""
import hashlib
import re
import sys

root, ts_path, rust_path, out = sys.argv[1:5]

ANCHOR = {
    "mapping": "### Per-Expression Mapping `->` (Rust only)",
    "mapping-target": "The right side of `->` is a value expression, optionally followed by `: Type`",
    "ws": "| `@ws` | `@ws /regex/ ;` | Override `?w` whitespace operator grammar-wide |",
    "token": "| `@token` | `@token ruleName ;` | Mark rule as lexical token (span-eligible, fusion-inlined, variant preserved) |",
    "inline-flags": "A pattern may begin with an inline flag group, `(?flags)`, whose flags (`i`, `m`, `s`, `u`) apply to the whole pattern.",
    "span": "`@{ expr }` matches `expr` and yields the source span it matched as its value",
    "juxtaposition": "The comma is optional when operands are unambiguous, but recommended for clarity.",
}
# Which constructs the spec spoke to before this unit (SPEC), and which sentences this unit adds (ADDED).
AUTHORITY = {"mapping": "SPEC", "mapping-target": "ADDED", "ws": "SPEC", "token": "SPEC",
             "inline-flags": "ADDED", "span": "ADDED", "juxtaposition": "SPEC"}


def read(p):
    v = {}
    for ln in open(p, encoding="utf-8"):
        parts = ln.rstrip("\n").split("\t")
        if len(parts) >= 2 and parts[1] in ("ACCEPT", "REJECT"):
            v[parts[0]] = (parts[1], parts[2] if len(parts) > 2 else "")
    return v


def constructs(src: str):
    body = re.sub(r'"(\\.|[^"\\])*"|\'(\\.|[^\'\\])*\'|`(\\.|[^`\\])*`', '""', src)  # drop literal contents
    body = re.sub(r"//[^\n]*|/\*.*?\*/", "", body, flags=re.S)
    found = []
    if "->" in body:
        found += ["mapping"]
        if re.search(r"->[^;|,]*?(\(|\|)", body) or re.search(r"->[^;]*?:\s*[A-Za-z_]", body):
            found += ["mapping-target"]
    if re.search(r"^\s*@ws\b", body, re.M):
        found += ["ws"]
    if re.search(r"/\(\?[imsu]+\)", body):
        found += ["inline-flags"]
    if re.search(r"^\s*@token\b", body, re.M):
        found += ["token"]
    if "@{" in body:
        found += ["span"]
    rules = re.findall(r"=([^;]*);", body)
    if any(re.search(r'(["/)\]}a-zA-Z0-9_])\s+(["/(\[{a-zA-Z_])', r) and "," not in r and "|" not in r for r in rules):
        found += ["juxtaposition"]
    return found


ts, rust = read(ts_path), read(rust_path)
manifest = open(f"{root}/grammar/MANIFEST", encoding="utf-8").read()
files = [ln.split()[1] for ln in manifest.splitlines() if ln.strip()]
rows = []
for f in files:
    t, r = ts[f][0], rust[f][0]
    if t == r:
        continue
    src = open(f"{root}/{f}", encoding="utf-8").read()
    if r == "ACCEPT":
        cs = constructs(src)
        anchors = " ‖ ".join(ANCHOR[c] for c in cs)
        auth = ",".join(sorted({AUTHORITY[c] for c in cs}))
        rows.append([f, "ACCEPT", "REJECT", "+".join(cs), auth, anchors, "CURED-TS"])
    else:
        rows.append([f, "REJECT", "ACCEPT", rust[f][1][:80], "SPEC", "", "TBD"])

with open(out, "w", encoding="utf-8") as o:
    o.write("# sk-v27 LW2.PAR-1 TS<->Rust divergences, ruled under R-b (spec text first; where silent, the compiler, ask S9-A)\n")
    o.write(f"# manifest-sha256\t{hashlib.sha256(manifest.encode()).hexdigest()}\n")
    o.write(f"# cases\t{len(files)}\n")
    o.write("# rust = BbnfBootstrap::parse (crates/bootstrap debug_parse); ts_base = typescript/ at the PAR-1 base; fate CURED-TS rows are\n")
    o.write("# cured in typescript/ by this unit, CARRIED rows are Rust-side (born-RED, LW3.COMP-1). anchor = exact docs/bbnf/ text, ' ‖ '-separated.\n")
    o.write("id\tfiles\trust\tts_base\tconstructs\tauthority\tanchor\tfate\n")
    for i, r in enumerate(rows, 1):
        o.write(f"D-{i:02d}\t" + "\t".join(r) + "\n")
print(len(rows), "rows")
