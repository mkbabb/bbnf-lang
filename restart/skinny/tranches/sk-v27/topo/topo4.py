#!/usr/bin/env python3
"""LW1.TOPO-4: divergence classification and the grammar MANIFEST.

  topo4.py divergence <repo> <local> <origin> <out.tsv>   write DIVERGENCE.tsv
  topo4.py manifest   <worktree> [--check]                 write or check grammar/MANIFEST
  topo4.py verify     <repo> <worktree> <local> <origin>   the falsifier

MANIFEST: one line per tracked *.bbnf, "sha256  path", sorted by path; it
excludes .claude/worktrees/, restart/ and target/."""
import hashlib, pathlib, re, subprocess, sys

EXCL = (".claude/worktrees/", "restart/", "target/")
TR = pathlib.Path(__file__).resolve().parent.parent

def git(d, *a):
    return subprocess.run(["git", "-C", str(d), *a], check=True, capture_output=True, text=True).stdout

def manifest_text(wt):
    paths = sorted(p for p in git(wt, "ls-files", "-z", "--", "*.bbnf").split("\0") if p and not p.startswith(EXCL))
    return "".join(f"{hashlib.sha256((pathlib.Path(wt) / p).read_bytes()).hexdigest()}  {p}\n" for p in paths)

def area(p):
    head, _, rest = p.partition("/")
    if head == "crates": return "crates/" + rest.split("/", 1)[0]
    return head if rest else p

def klass(areas):
    a = set(areas)
    if not a: return "merge"
    if a <= {"restart", "docs"}: return "record"
    if a <= {"restart", "docs", "skinny"}: return "skinny-lab"
    if a <= {"typescript", ".github", ".gitignore"}: return "ts-package" if "typescript" in a else "ci"
    if any(x.startswith("crates/") for x in a): return "compiler-code"
    return "tooling"

def divergence(repo, local, origin):
    rows = []
    for side, rng in (("local-only", f"{origin}..{local}"), ("origin-only", f"{local}..{origin}")):
        for line in git(repo, "log", "--format=%H\x1f%cs\x1f%P\x1f%s", rng).splitlines():
            sha, date, parents, subj = line.split("\x1f")
            files = git(repo, "show", "--format=", "--name-only", sha).split() if len(parents.split()) == 1 else []
            areas = sorted({area(f) for f in files})
            rows.append((side, sha, date, klass(areas), ",".join(areas) or "-", subj.replace("\t", " ")))
    return rows

cmd = sys.argv[1]
if cmd == "divergence":
    repo, local, origin, out = sys.argv[2:6]
    rows = divergence(repo, local, origin)
    pathlib.Path(out).write_text("side\tcommit\tdate\tclass\tareas\tsubject\n" + "".join("\t".join(r) + "\n" for r in rows))
    print(len(rows), "rows")
elif cmd == "manifest":
    wt = sys.argv[2]; f = pathlib.Path(wt) / "grammar" / "MANIFEST"
    t = manifest_text(wt)
    if "--check" in sys.argv:
        sys.exit(0 if f.is_file() and f.read_text() == t else 1)
    f.write_text(t); print(len(t.splitlines()), "grammars", hashlib.sha256(t.encode()).hexdigest())
elif cmd == "verify":
    repo, wt, local, origin = sys.argv[2:6]
    bad = []
    f = pathlib.Path(wt) / "grammar" / "MANIFEST"
    if not f.is_file(): bad.append("grammar/MANIFEST absent")
    else:
        if f.read_text() != manifest_text(wt): bad.append("grammar/MANIFEST does not match the tree")
        h = hashlib.sha256(f.read_bytes()).hexdigest()
        if not re.search(rf"LG-b[^\n]*{h}", (TR / "LEDGER.md").read_text()):
            bad.append(f"no LG-b ledger row cites MANIFEST sha256 {h[:12]}")
    d = TR / "evidence" / "LW1.TOPO" / "DIVERGENCE.tsv"
    if not d.is_file(): bad.append("DIVERGENCE.tsv absent")
    else:
        got = {}
        for r in d.read_text().splitlines()[1:]:
            side, sha, _, cls, *_ = r.split("\t")
            if not cls: bad.append(f"{sha[:9]} unclassified")
            got.setdefault(side, set()).add(sha)
        for side, rng in (("local-only", f"{origin}..{local}"), ("origin-only", f"{local}..{origin}")):
            want = set(git(repo, "rev-list", rng).split())
            if got.get(side, set()) != want:
                bad.append(f"{side}: {len(got.get(side, ()))} classified vs {len(want)} in git")
    print("RED" if bad else "GREEN", *bad, sep="\n  ")
    sys.exit(1 if bad else 0)
