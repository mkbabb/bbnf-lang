#!/usr/bin/env python3
"""LW3 entry `#[ignore]` census (LW3-ORDER amendment 3; read by LG-3.11).

Every attribute matching `^\\s*#\\[ignore` in every tracked file of bbnf-lang,
parse-that and pprint at the `sk-v27-lw3` heads that heads-LW3.json records,
by repo, file, line, test id and reason. Doc comments that mention `#[ignore]`
do not match the anchored pattern and are not counted.

usage: topo0_lw3_ignore_census.py <heads-LW3.json> <out.tsv>"""
import json, pathlib, re, subprocess, sys

P = pathlib.Path.home() / "Programming"
heads = json.loads(pathlib.Path(sys.argv[1]).read_text())["repos"]
out = pathlib.Path(sys.argv[2])
ATTR = re.compile(r"^\s*#\[ignore")
FN = re.compile(r"^\s*(?:pub\s+)?(?:async\s+)?fn\s+(\w+)")


def git(repo, *a, ok=(0,)):
    p = subprocess.run(["git", "-C", str(P / repo), *a], capture_output=True, text=True)
    if p.returncode not in ok:
        raise RuntimeError(f"{repo} {a}: rc {p.returncode} {p.stderr[:200]}")
    return p.stdout


rows, per = [], {}
for repo in ("bbnf-lang", "parse-that", "pprint"):
    rev = heads[repo]["remote"]["refs/heads/sk-v27-lw3"]
    hits = git(repo, "grep", "-nP", r"^\s*#\[ignore", rev, "--", ".", ok=(0, 1)).splitlines()
    files = {}
    for h in hits:
        _, path, line, _ = h.split(":", 3)
        files.setdefault(path, []).append(int(line))
    for path, lines in sorted(files.items()):
        text = git(repo, "show", f"{rev}:{path}").splitlines()
        for ln in lines:
            attr = text[ln - 1]
            j, buf = ln - 1, attr
            while "]" not in buf.split("#[ignore", 1)[1] and j + 1 < len(text):  # a multi-line reason
                j += 1; buf += " " + text[j].strip()
            m = re.search(r'#\[ignore\s*(?:=\s*"((?:[^"\\]|\\.)*)")?', buf)
            reason = (m.group(1) if m and m.group(1) is not None else "(no reason)").replace("\t", " ")
            test = "(no fn)"
            for k in range(j + 1, min(j + 12, len(text))):
                f = FN.match(text[k])
                if f:
                    test = f.group(1); break
            rows.append((repo, rev[:9], path, str(ln), test, reason))
            per[repo] = per.get(repo, 0) + 1
out.write_text("repo\trev\tfile\tline\ttest\treason\n" + "".join("\t".join(r) + "\n" for r in rows))
print(f"{len(rows)} attributes: " + ", ".join(f"{k} {v}" for k, v in per.items()))
print(f"of which under restart/ or archives: {sum(1 for r in rows if r[2].startswith('restart/'))}")
