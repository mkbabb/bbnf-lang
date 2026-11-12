#!/usr/bin/env python3
"""LW3.LED-1 read: the pprint PR erratum to LED:258 ("pprint #5 stays open").

(1) `gh pr list --repo mkbabb/pprint --state all` -> pprint-prs.json; PRs #2..#5
    with their state, mergedAt and mergeCommit.
(2) `git cat-file -t` of each merge commit, and whether it is an ancestor of
    pprint origin/master.
(3) Whether master's tree changed by the three extra merges: master's tree
    against the #3 merge commit's tree (`git diff dccbd63 <#3 merge>`).
(4) TOPO-4-A5 re-verified: each of the 14 crates LW2 published is downloaded
    again from crates.io and its sha256 compared with LW2.GATE/r8's
    published-served.tsv; the pprint 0.4.0 and pprint_derive 0.3.0 crates'
    `.cargo_vcs_info.json` names 32d73ca, and every packaged source file equals
    `git show 32d73ca:<path>` byte for byte (cargo's rewritten manifest and
    its own metadata files excepted, and named).

usage: led1_read.py <tranche-dir> <out-prefix>   (writes <out-prefix>.json/.txt)"""
import hashlib, io, json, pathlib, subprocess, sys, tarfile, urllib.request

TR = pathlib.Path(sys.argv[1]); OUT = pathlib.Path(sys.argv[2])
PP = pathlib.Path.home() / "Programming/pprint"
lines, ok = [], True


def say(s):
    lines.append(s)


def git(*a):
    return subprocess.run(["git", "-C", str(PP), *a], capture_output=True, text=True)


prs = json.loads(subprocess.run(["gh", "pr", "list", "--repo", "mkbabb/pprint", "--state", "all", "--json",
                                 "number,state,mergedAt,mergeCommit,headRefName,baseRefName"],
                                capture_output=True, text=True, check=True).stdout)
OUT.with_suffix(".json").write_text(json.dumps(sorted(prs, key=lambda p: p["number"]), indent=1) + "\n")
want = {2: "ec47846", 3: "dccbd634", 4: "32e58de3", 5: "bc9d47fe"}
by = {p["number"]: p for p in prs}
for n, pre in want.items():
    p = by.get(n)
    oid = (p or {}).get("mergeCommit", {}) or {}
    oid = oid.get("oid", "")
    good = bool(p) and p["state"] == "MERGED" and oid.startswith(pre)
    t = git("cat-file", "-t", oid).stdout.strip() if oid else "absent"
    anc = git("merge-base", "--is-ancestor", oid, "origin/master").returncode == 0 if oid else False
    good = good and t == "commit" and anc
    ok &= good
    say(f"pprint #{n}: {p and p['state']} at {p and p['mergedAt']} mergeCommit {oid[:9]} "
        f"(order: {pre}) cat-file={t} ancestor-of-master={anc} -> {'OK' if good else 'MISMATCH'}")
master = git("rev-parse", "origin/master").stdout.strip()
d = git("diff", "--stat", master, by[3]["mergeCommit"]["oid"]).stdout.strip()
say(f"master {master[:9]} vs the #3 merge {by[3]['mergeCommit']['oid'][:9]}: "
    f"{'tree unchanged (empty diff)' if not d else 'TREE DIFFERS: ' + d}")
say("  reading: #2, #4 and #5 made no commit of their own; GitHub recorded each as merged when its head became "
    "reachable from master through #3's merge, and records that head as the PR's mergeCommit.")
ok &= not d

served = {}
for row in (TR / "evidence/LW2.GATE/r8/published-served.tsv").read_text().splitlines():
    f = row.split("\t")
    served[(f[0], f[1])] = f[4].split("=", 1)[1]
crates = {}
for (name, ver), sha in sorted(served.items()):
    req = urllib.request.Request(f"https://static.crates.io/crates/{name}/{name}-{ver}.crate",
                                 headers={"User-Agent": "lockstep-ledger-read"})
    body = urllib.request.urlopen(req, timeout=60).read()
    got = hashlib.sha256(body).hexdigest()
    crates[name] = body
    ok &= got == sha
    say(f"{name} {ver}: served sha256 {got[:16]}… {'== r8' if got == sha else '!= r8 ' + sha[:16]}")

for name, ver, sub in (("pprint", "0.4.0", "rust"), ("pprint_derive", "0.3.0", "rust/pprint_derive")):
    tf = tarfile.open(fileobj=io.BytesIO(crates[name]))
    root = f"{name}-{ver}/"
    vcs = json.loads(tf.extractfile(root + ".cargo_vcs_info.json").read())
    sha1, path_in_vcs = vcs["git"]["sha1"], vcs.get("path_in_vcs", "")
    good = sha1.startswith("32d73ca")
    same, differ, skipped = 0, [], []
    # A `readme` outside the package directory is packaged at the crate root under
    # its file name (cargo's rule); it is compared against the file it names.
    import posixpath, re as _re
    rd = _re.search(r'^readme\s*=\s*"([^"]+)"', tf.extractfile(root + "Cargo.toml.orig").read().decode(), _re.M)
    readme = {posixpath.basename(rd.group(1)): posixpath.normpath(posixpath.join(path_in_vcs, rd.group(1)))} if rd else {}
    for m in tf.getmembers():
        if not m.isfile():
            continue
        rel = m.name[len(root):]
        if rel in ("Cargo.toml", "Cargo.toml.orig", ".cargo_vcs_info.json", "Cargo.lock"):
            skipped.append(rel); continue
        gpath = readme.get(rel) or (f"{path_in_vcs}/{rel}" if path_in_vcs else rel)
        g = subprocess.run(["git", "-C", str(PP), "show", f"32d73ca:{gpath}"], capture_output=True)
        if g.returncode == 0 and g.stdout == tf.extractfile(m).read():
            same += 1
        else:
            differ.append(gpath)
    orig = tf.extractfile(root + "Cargo.toml.orig").read()
    g = subprocess.run(["git", "-C", str(PP), "show", f"32d73ca:{path_in_vcs}/Cargo.toml"], capture_output=True)
    orig_ok = g.returncode == 0 and g.stdout == orig
    good = good and not differ and orig_ok
    ok &= good
    say(f"{name} {ver}: vcs sha1 {sha1[:9]} path_in_vcs={path_in_vcs!r}; {same} source files == 32d73ca, "
        f"{len(differ)} differ{(' ' + ', '.join(differ)) if differ else ''}; Cargo.toml.orig == 32d73ca:{path_in_vcs}/Cargo.toml: "
        f"{orig_ok}; cargo-written files not compared: {', '.join(sorted(skipped))} -> {'OK' if good else 'MISMATCH'}")
say("GREEN" if ok else "RED")
OUT.with_suffix(".txt").write_text("\n".join(lines) + "\n")
print("\n".join(lines))
sys.exit(0 if ok else 1)
